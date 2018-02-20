#ifndef RCPP_MATSLICES_H
#define RCPP_MATSLICES_H


//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>
#include<optional>


class MatSlices{
private:

  Rcpp::CharacterVector filenames;
  Rcpp::CharacterVector groupnames;
  Rcpp::CharacterVector datanames;
  Rcpp::IntegerVector row_offsets;
  Rcpp::IntegerVector col_offsets;
  Rcpp::IntegerVector chunk_group;
  Rcpp::IntegerVector col_chunksizes;
  Rcpp::IntegerVector row_chunksizes;
  std::unordered_map<std::string,std::shared_ptr<HighFive::File> >  &m_file_map;
  std::unordered_map<std::string,std::shared_ptr<HighFive::Group> >  &m_group_map;
  std::unordered_map<std::string,std::shared_ptr<HighFive::DataSet> > &m_dataset_map;
  size_t num_reg;
  size_t num_chunks;
  const bool readOnly;
  bool has_col(Rcpp::CharacterVector colnames,const std::string tcol){
    for(auto tc:colnames){
      if(Rcpp::as<std::string>(tc)==tcol){
        return(true);
      }
    }
    return(false);
  }
public:
  MatSlices(const Rcpp::DataFrame dff,
            std::unordered_map<std::string,std::shared_ptr<HighFive::File> >&m_file_map_,
            std::unordered_map<std::string,std::shared_ptr<HighFive::Group> >&m_group_map_,
            std::unordered_map<std::string,std::shared_ptr<HighFive::DataSet> > &m_dataset_map_,
            const bool readOnly_=true
  ):
  num_reg(dff.rows()),
  readOnly(readOnly_),
  m_file_map(m_file_map_),
  m_group_map(m_group_map_),
  m_dataset_map(m_dataset_map_)
  {
    Rcpp::Function structure("str");

    Rcpp::CharacterVector colnames = dff.names();
    if(!has_col(colnames,"filenames")){
      structure(dff);
      Rcpp::stop("dataframe is missing filenames!");
    }
    if(!has_col(colnames,"groupnames")){
      structure(dff);
      Rcpp::stop("dataframe is missing groupnames!");
    }
    if(!has_col(colnames,"datanames")){
      structure(dff);
      Rcpp::stop("dataframe is missing datanames!");
    }
    filenames=dff["filenames"];
    groupnames=dff["groupnames"];
    datanames=dff["datanames"];
    if(!has_col(colnames,"chunk_group")){
      chunk_group=Rcpp::IntegerVector(num_reg);
      num_chunks=num_reg;
      std::iota(chunk_group.begin(),chunk_group.end(),0);
    }else{
      chunk_group=dff["chunk_group"];
      std::set<int> tcset;
      for(auto cg:chunk_group){
        tcset.insert(cg);
      }
      num_chunks=tcset.size();
    }
    chunk_map.reserve(num_chunks);
    row_offsets = has_col(colnames,"row_offsets") ? dff["row_offsets"] : Rcpp::IntegerVector(num_reg,0);
    col_offsets = has_col(colnames,"col_offsets") ? dff["col_offsets"] : Rcpp::IntegerVector(num_reg,0);
    row_chunksizes = has_col(colnames,"row_chunksizes") ? dff["row_chunksizes"] : Rcpp::IntegerVector(num_reg,-1);
    col_chunksizes = has_col(colnames,"col_chunksizes") ? dff["col_chunksizes"] : Rcpp::IntegerVector(num_reg,-1);

    using namespace HighFive;
    auto rt = readOnly ? File::ReadOnly : File::ReadWrite | File::Create;
    std::string tfn,tgn,tdn;
    std::vector<size_t> data_dimv;
    int tgc;
    for(int i=0; i<num_reg;i++){

      tfn=Rcpp::as<std::string>(filenames(i));
      tgn=Rcpp::as<std::string>(groupnames(i));
      tdn=Rcpp::as<std::string>(datanames(i));
      tgc=chunk_group(i);

      auto gmf=chunk_map[tgc].find(tdn);
      if(gmf!=chunk_map[tgc].end()){
        Rcpp::Rcerr<<"In row: "<<i<<std::endl;
        Rcpp::Rcerr<<"In chunk: "<<tgc<<std::endl;
        Rcpp::Rcerr<<tfn<<"/"<<tgn<<"/"<<tdn<<std::endl;
        Rcpp::stop("duplicate dataset in read/write chunk! Each read write chunk must contain only one reference to a given dataset");
      }else{
        chunk_map[tgc].emplace_hint(gmf,tdn,i);
      }

      auto mtf = m_file_map.find(tfn);
      if(mtf==m_file_map.end()){
        mtf = m_file_map.emplace_hint(mtf,tfn,std::make_shared<File>(tfn,rt));
      }

      std::string g_arr=tfn+tgn;
      auto mtg = m_group_map.find(g_arr);

      if(mtg==m_group_map.end()){
        mtg = m_group_map.emplace_hint(mtg,std::move(g_arr),std::make_shared<Group>(mtf->second->getGroup(tgn)));
      }

      std::string d_arr=tfn+tgn+tdn;
      auto mtd = m_dataset_map.find(d_arr);

      if(mtd==m_dataset_map.end()){
        mtd = m_dataset_map.emplace_hint(mtd,std::move(d_arr),std::make_shared<DataSet>(mtg->second->getDataSet(tdn)));
      }

      data_dimv=mtd->second->getDataDimensions();

      if(row_chunksizes(i)<0){
        row_chunksizes(i)=data_dimv[0]-row_offsets(i);
      }
      if(data_dimv.size()==1){
        data_dimv.push_back(1);
      }
      if(col_chunksizes(i)<0){
        col_chunksizes(i)=data_dimv[1]-col_offsets(i);
      }
    }
  }
  template<typename T, int RAC, int CAC, int Options> void write(const int i,Eigen::Matrix<T,RAC,CAC,Options>& b){
    std::string data_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i))+Rcpp::as<std::string>(datanames(i));

    auto mtd = m_dataset_map.find(data_arr);
    mtd->second->selectEigen({static_cast<size_t>(row_offsets(i)),static_cast<size_t>(col_offsets(i))},{static_cast<size_t>(row_chunksizes(i)),static_cast<size_t>(col_chunksizes(i))},{}).write(b);
  }
  template<typename T, int RAC, int CAC, int Options> void write_chunk(const int i,const std::string& dataname,Eigen::Matrix<T,RAC,CAC,Options>& b){
    write(chunk_map.at(i).at(dataname),b);
  }
  template<typename T, int RAC, int CAC, int Options> void read(const int i,Eigen::Matrix<T,RAC,CAC,Options>& b){
    std::string data_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i))+Rcpp::as<std::string>(datanames(i));
    auto mtd = m_dataset_map.find(data_arr);
    mtd->second->selectEigen({static_cast<size_t>(row_offsets(i)),static_cast<size_t>(col_offsets(i))},{static_cast<size_t>(row_chunksizes(i)),static_cast<size_t>(col_chunksizes(i))},{}).read(b);
  }
  template<typename T,typename A> void read_vector(const int i, std::vector<T,A> &b){
    b.resize(row_chunksizes(i));
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tb(b.data(),row_chunksizes(i),1);
    std::string data_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i))+Rcpp::as<std::string>(datanames(i));
    auto mtd = m_dataset_map.find(data_arr);
    mtd->second->selectEigen({static_cast<size_t>(row_offsets(i)),static_cast<size_t>(col_offsets(i))},{static_cast<size_t>(row_chunksizes(i)),static_cast<size_t>(col_chunksizes(i))},{}).read(tb);
  }
  template<typename T,typename A> void write_vector(const int i, std::vector<T,A> &b){
    //b.resize(row_chunksizes(i));
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tb(b.data(),row_chunksizes(i),1);
    std::string data_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i))+Rcpp::as<std::string>(datanames(i));
    auto mtd = m_dataset_map.find(data_arr);
    mtd->second->selectEigen({static_cast<size_t>(row_offsets(i)),static_cast<size_t>(col_offsets(i))},{static_cast<size_t>(row_chunksizes(i)),static_cast<size_t>(col_chunksizes(i))},{}).write(tb);
  }


  template<typename T,typename A> void read_chunk_vector(const int i,const std::string& dataname, std::vector<T,A> &b){
    read_vector(chunk_map.at(i).at(dataname),b);
  }
  template<typename T,typename A> void write_chunk_vector(const int i,const std::string& dataname, std::vector<T,A> &b){
    write_vector(chunk_map.at(i).at(dataname),b);
  }


  template<typename T, int RAC, int CAC, int Options> void read_chunk(const int i,const std::string& dataname,Eigen::Matrix<T,RAC,CAC,Options>& b){
    read(chunk_map.at(i).at(dataname),b);
  }

  std::vector<int> dims(const int i){
    std::vector<int> retvec={row_chunksizes(i),col_chunksizes(i)};
    return(retvec);
  }
  std::vector<int> dims_chunk(const int i,std::string dataname){
    return(dims(chunk_map.at(i).at(dataname)));
  }
  std::unordered_map<int,std::unordered_map<std::string,int> > chunk_map;
  bool p_first;
  int p;
  int N;
  };

#endif
