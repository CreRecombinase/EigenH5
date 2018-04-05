#ifndef RCPP_MATSLICES_H
#define RCPP_MATSLICES_H


//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>




// class MatSelection{
//   std::vector<std::array<size_t,2> > offsets;
//   std::vector<std::array<size_t,2> > chunksizes;
//   std::vector<std::array<bool,2> > flips;
//   size_t rownum;
//   size_t colnum;
//   MatSelection(Rcpp::IntegerVector row_selection,

// }


















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
  Rcpp::LogicalVector create_dset;
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
  int chunk_map_i(int i, const std::string dataname){
    auto chunk_r=chunk_map.find(i);
    if(chunk_r==chunk_map.end()){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::stop("chunk_map i not found!");
    }
    auto dnr = chunk_r->second.find(dataname);
    if(dnr==chunk_r->second.end()){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Can't find dataname: "<<dataname<<std::endl;
      Rcpp::stop("Can't perform read!");
    }
    return(dnr->second);
  }
  std::shared_ptr<HighFive::Group> get_slice_group(int i){
    if(i>num_reg || i<0){
      Rcpp::Rcerr<<"In function get_slice"<<std::endl;
      Rcpp::Rcerr<<"i: "<<i<<" is larger than "<<num_reg<<std::endl;
      Rcpp::stop("invalid selection!");
    }
    
    std::string group_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i));
    auto mtg = m_group_map.find(group_arr);
    if(mtg==m_group_map.end()){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<group_arr<<std::endl;
      Rcpp::stop("group_map i not found!");
    }
    return(mtg->second);
  }
  
  HighFive::Selection get_slice(int i){
    if(i>num_reg || i<0){
      Rcpp::Rcerr<<"In function get_slice"<<std::endl;
      Rcpp::Rcerr<<"i: "<<i<<" is larger than "<<num_reg<<std::endl;
      Rcpp::stop("invalid selection!");
    }
    
    std::string data_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i))+Rcpp::as<std::string>(datanames(i));
    auto mtd = m_dataset_map.find(data_arr);
    if(mtd==m_dataset_map.end()){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::stop("dataset_map i not found!");
    }
    if(row_offsets(i)<0){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::Rcerr<<"Invalid row_offset: "<<row_offsets(i)<<"!"<<std::endl;
      Rcpp::stop("row_offset(i) must be non-negative!");
    }
    if(col_offsets(i)<0){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::Rcerr<<"Invalid col_offset: !"<<col_offsets(i)<<std::endl;
      Rcpp::stop("col_offset(i) must be non-negative!");
    }
    if(col_chunksizes(i)<0){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::Rcerr<<"Invalid col_chunksizes: !"<<col_chunksizes(i)<<std::endl;
      Rcpp::stop("col_chunksizes(i) must be non-negative!");
    }
    if(row_chunksizes(i)<0){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::Rcerr<<"Invalid row_chunksizes: !"<<row_chunksizes(i)<<std::endl;
      Rcpp::stop("row_chunksizes(i) must be non-negative!");
    }
  return(mtd->second->selectEigen({static_cast<size_t>(row_offsets(i)),static_cast<size_t>(col_offsets(i))},{static_cast<size_t>(row_chunksizes(i)),static_cast<size_t>(col_chunksizes(i))},{}));
  }
  HighFive::Selection get_vec_slice(int i){
    if(i>num_reg || i<0){
      Rcpp::Rcerr<<"In function get_slice_vec"<<std::endl;
      Rcpp::Rcerr<<"i: "<<i<<" is larger than "<<num_reg<<std::endl;
      Rcpp::stop("invalid selection!");
    }
    std::string data_arr=Rcpp::as<std::string>(filenames(i))+Rcpp::as<std::string>(groupnames(i))+Rcpp::as<std::string>(datanames(i));
    auto mtd = m_dataset_map.find(data_arr);
    if(mtd==m_dataset_map.end()){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::stop("dataset_map i not found!");
    }
    if(row_offsets(i)<0){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::Rcerr<<"Invalid row_offset: "<<row_offsets(i)<<"!"<<std::endl;
      Rcpp::stop("row_offset(i) must be non-negative!");
    }
    if(row_chunksizes(i)<0){
      Rcpp::Rcerr<<"In chunk_group: "<<i<<std::endl;
      Rcpp::Rcerr<<"Path: "<<data_arr<<std::endl;
      Rcpp::Rcerr<<"Invalid row_chunksizes: !"<<row_chunksizes(i)<<std::endl;
      Rcpp::stop("row_chunksizes(i) must be non-negative!");
    }
    return(mtd->second->select({static_cast<size_t>(row_offsets(i))},{static_cast<size_t>(row_chunksizes(i))},{}));
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
    //chunk_map.reserve(num_chunks);
    create_dset = has_col(colnames,"create_dynamic") ? dff["create_dynamic"] : Rcpp::LogicalVector(num_reg,false);
    row_offsets = has_col(colnames,"row_offsets") ? dff["row_offsets"] : Rcpp::IntegerVector(num_reg,0);
    col_offsets = has_col(colnames,"col_offsets") ? dff["col_offsets"] : Rcpp::IntegerVector(num_reg,0);
    row_chunksizes = has_col(colnames,"row_chunksizes") ? dff["row_chunksizes"] : Rcpp::IntegerVector(num_reg,-1);
    col_chunksizes = has_col(colnames,"col_chunksizes") ? dff["col_chunksizes"] : Rcpp::IntegerVector(num_reg,-1);

    using namespace HighFive;
    auto rt = readOnly ? File::ReadOnly : File::ReadWrite | File::Create;
    std::string tfn,tgn,tdn;
    std::vector<size_t> data_dimv;
    int tgc;
    bool dyn_create;
    for(int i=0; i<num_reg;i++){

      tfn=Rcpp::as<std::string>(filenames(i));
      tgn=Rcpp::as<std::string>(groupnames(i));
      tdn=Rcpp::as<std::string>(datanames(i));
      tgc=chunk_group(i);
      dyn_create = create_dset(i);
      

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
        if(!dyn_create){
          mtg = m_group_map.emplace_hint(mtg,std::move(g_arr),std::make_shared<Group>(mtf->second->getGroup(tgn)));
        }else{
          mtg = m_group_map.emplace_hint(mtg,std::move(g_arr),std::make_shared<Group>(mtf->second->createOrGetGroups(tgn)));
        }
      }

      std::string d_arr=tfn+tgn+tdn;
      auto mtd = m_dataset_map.find(d_arr);

      if(mtd==m_dataset_map.end()){
        if(!dyn_create){
          mtd = m_dataset_map.emplace_hint(mtd,std::move(d_arr),std::make_shared<DataSet>(mtg->second->getDataSet(tdn)));
        }else{
          mtd = m_dataset_map.emplace_hint(mtd,std::move(d_arr),std::shared_ptr<DataSet>());
        }
      }
      if(!dyn_create){
        data_dimv=mtd->second->getDataDimensions();
        
        if(row_chunksizes(i)<0){
          row_chunksizes(i)=data_dimv[0]-row_offsets(i);
        }
        if(data_dimv.size()==1){
          data_dimv.push_back(1);
        }
        if(col_chunksizes(i)<0){
          if(data_dimv.size()==1){
            col_chunksizes(i)=1;
          }else{
            col_chunksizes(i)=data_dimv[1]-col_offsets(i);
          }
        }
      }
    }
  }
  template<typename T, int RAC, int CAC, int Options> void write(const int i,Eigen::Matrix<T,RAC,CAC,Options>& b){
    using namespace HighFive;
    const bool d_c=create_dset(i);
    if(!d_c){
    get_slice(i).write(b);
    }else{
      std::string dataname=Rcpp::as<std::string>(datanames(i));
      auto mtg = get_slice_group(i);
      std::vector<size_t> mat_dims={static_cast<size_t>(b.rows()),static_cast<size_t>(b.cols())};
      std::vector<size_t> chunk_dims(2);
      const size_t MAX_CHUNK = 1024*1024;
      const size_t chunk_rows = static_cast<size_t>(std::min(static_cast<double>(b.rows()),std::ceil(static_cast<double>(MAX_CHUNK)/static_cast<double>(b.cols()))));
      chunk_dims = {chunk_rows, static_cast<size_t>(b.cols())};
      Filter filter(chunk_dims, FILTER_BLOSC, 0);
      DataSpace ds = DataSpace(mat_dims);
      DataSet dataset = mtg->createDataSet(dataname, ds, AtomicType<T>(), filter.getId(), false);
      dataset.write(b);
    }
  }
  
  // template<typename T, int RAC, int CAC, int Options> void write_create(const int i,Eigen::Matrix<T,RAC,CAC,Options>& b){
  //   
  //   get_slice(i).write(b);
  // }

  template<typename T, int RAC, int CAC, int Options> void read(const int i,Eigen::Matrix<T,RAC,CAC,Options>& b){
    // 2147483631
    get_slice(i).read(b);
  }
  template<typename T,typename A> void read_vector(const int i, std::vector<T,A> &b){
    b.resize(row_chunksizes(i));
    // Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tb(b.data(),row_chunksizes(i),1);
    get_vec_slice(i).read(b);
  }
  template<typename T,typename A> void write_vector(const int i, std::vector<T,A> &b){
    using namespace HighFive;
    const bool d_c=create_dset(i);
    if(!d_c){
      get_vec_slice(i).write(b);
    }else{
      std::string dataname=Rcpp::as<std::string>(datanames(i));
      auto mtg = get_slice_group(i);
      std::vector<size_t> vec_dims={static_cast<size_t>(b.size())};
      std::vector<size_t> chunk_dims(1);
      const size_t MAX_CHUNK = (1024*1024)/2;
      const size_t chunk_rows = static_cast<size_t>(std::min(static_cast<double>(b.size()),std::ceil(static_cast<double>(MAX_CHUNK))));
      chunk_dims = {chunk_rows};
      Filter filter(chunk_dims, FILTER_BLOSC, 0);
      DataSpace ds = DataSpace(vec_dims);
      DataSet dataset = mtg->createDataSet(dataname, ds, AtomicType<T>(), filter.getId(), false);
      dataset.write(b);
    }
  }
  template<typename T,typename A> void read_chunk_vector(const int i,const std::string& dataname, std::vector<T,A> &b){
    read_vector(chunk_map_i(i,dataname),b);
  }
  template<typename T,typename A> void write_chunk_vector(const int i,const std::string& dataname, std::vector<T,A> &b){
    write_vector(chunk_map_i(i,dataname),b);
  }
  template<typename T, int RAC, int CAC, int Options> void read_chunk(const int i,const std::string& dataname,Eigen::Matrix<T,RAC,CAC,Options>& b){
    read(chunk_map_i(i,dataname),b);
  }
  template<typename T, int RAC, int CAC, int Options> void write_chunk(const int i,const std::string& dataname,Eigen::Matrix<T,RAC,CAC,Options>& b){
    write(chunk_map_i(i,dataname),b);
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
