#include <EigenH5.h>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp14)]]
#include <highfive/H5DataSet.hpp>
#include <highfive/H5Filter.hpp>
#include <highfive/H5DataSpace.hpp>
#include <highfive/H5File.hpp>
#include <highfive/H5Attribute.hpp>
#include <highfive/H5Utility.hpp>
#include <highfive/H5DataType.hpp>
#include <highfive/H5Group.hpp>
#include <highfive/H5PropertyList.hpp>
#include <highfive/H5FileDriver.hpp>
#include <highfive/H5Object.hpp>
#include <highfive/H5Selection.hpp>
#include <blosc_filter.h>
#include <array>
#include<H5Tpublic.h>


using selection_tup=std::tuple<std::vector<size_t>,std::vector<size_t>,std::vector<size_t> >;

template<typename T,size_t D> std::map<T,selection_tup> split_LD(const std::array<size_t,D> data_dims, const std::vector<T> &region_ids){

  std::map<T,selection_tup> retmap;
  size_t num_SNPs=region_ids.size();
  const bool SNP_first=data_dims[0]==num_SNPs;
  constexpr bool is_mat = D==2;
  const size_t other_dim = is_mat ? (SNP_first ? data_dims[1] : data_dims[0]) : 0;
  if (is_mat){
    if(!SNP_first){
      if(data_dims[1]!=num_SNPs){
        Rcpp::stop("region_id dims don't match data dims");
      }
      assert(data_dims[1]==num_SNPs);
    }
  }else{
    assert(SNP_first);
  }
  size_t chunksize=0;
  size_t offset = 0;
  auto ld_region_begin = region_ids.begin();
  auto ld_region_end = region_ids.end();
  T check_reg;
  
  auto reg_range = std::make_pair(ld_region_begin,ld_region_begin);
  while(reg_range.second!=ld_region_end){
    check_reg=*(reg_range.second);
    reg_range = std::equal_range(region_ids.begin(),region_ids.end(),check_reg);
    if(reg_range.second!=reg_range.first){
      chunksize = reg_range.second-reg_range.first;
      offset = reg_range.first-ld_region_begin;
      if(is_mat){
        if(SNP_first){
          retmap.emplace(std::make_pair(*(reg_range.first),
                                        std::make_tuple(std::vector<size_t>({offset,0}),std::vector<size_t>({chunksize,other_dim}),std::vector<size_t>({}))));
        }else{
          retmap.emplace(std::make_pair(*(reg_range.first),
                                        std::make_tuple(std::vector<size_t>({0,offset}),std::vector<size_t>({other_dim,chunksize}),std::vector<size_t>({}))));
        }
      }else{
        retmap.emplace(std::make_pair(*(reg_range.first),
                                      std::make_tuple(std::vector<size_t>({offset}),std::vector<size_t>({chunksize}),std::vector<size_t>({}))));
      }
      
    }
  }
  
    return(retmap);
}

Rcpp::ListOf<Rcpp::NumericMatrix> read_split(const Rcpp::StringVector &matrix_path,const Rcpp::StringVector &region_path, const Rcpp::IntegerVector sub_regions=Rcpp::IntegerVector::create()){
  
  using namespace HighFive;
  using namespace Rcpp;
    std::vector<int> region_id_l;
    {
      File file(as<std::string>(region_path[0]),File::ReadOnly);
      file.getGroup(as<std::string>(region_path[1])).getDataSet(as<std::string>(region_path[2])).read(region_id_l);
    }
    File mat_file(as<std::string>(matrix_path[0]),File::ReadOnly);
    auto mat_dataset = mat_file.getGroup(as<std::string>(matrix_path[1])).getDataSet(as<std::string>(matrix_path[2]));
    auto dim_v = mat_dataset.getDataDimensions();
    std::array<size_t,2> mat_dims;
    mat_dims[0]=dim_v[0];
    mat_dims[1]=dim_v[1];
    auto map_t = split_LD(mat_dims,region_id_l);
    std::vector<int> check_regions(sub_regions.begin(),sub_regions.end());
    if(check_regions.empty()){
      size_t num_reg=map_t.size();
      Rcpp::ListOf<Rcpp::NumericMatrix> matlist;
      int idx=0;
      for(auto ldi = map_t.begin(); ldi!=map_t.end();ldi++){
        
        //retmat(idx,0)=ldi->first;
        auto val= ldi->second;
        auto offsetvec = std::get<0>(val);
        auto sizevec = std::get<1>(val);
        const size_t trows=sizevec[0];
        const size_t tcols=sizevec[1];
        
        auto ncvec = std::get<2>(val);
        matlist[idx]=Rcpp::NumericMatrix(trows,tcols);
        Eigen::Map<Eigen::MatrixXd> readmap(&(matlist[idx])(0,0),trows,tcols);
        mat_dataset.selectEigen(offsetvec,sizevec,ncvec).read(readmap);
        idx++;
      }
      return(matlist);
    }else{
      size_t num_reg=check_regions.size();
      Rcpp::ListOf<Rcpp::NumericMatrix> matlist(num_reg);
      int idx=0;
      for(auto val:check_regions){
        
        auto ldi = map_t.find(val);
        if(ldi!=map_t.end()){
        //retmat(idx,0)=ldi->first;
          auto valvec= ldi->second;
          auto offsetvec = std::get<0>(valvec);
          auto sizevec = std::get<1>(valvec);
          auto ncvec = std::get<2>(valvec);
          const size_t trows=sizevec[0];
          const size_t tcols=sizevec[1];
          
          matlist[idx]=Rcpp::NumericMatrix(trows,tcols);
          Eigen::Map<Eigen::MatrixXd> readmap(&(matlist[idx])(0,0),trows,tcols);
          mat_dataset.selectEigen(offsetvec,sizevec,ncvec).read(readmap);
        }else{
          Rcpp::stop("ld_region not found:"+std::to_string(val));
        }
        idx++;
      }
      return(matlist);
    }
  
}

//[[Rcpp::export]]
Rcpp::IntegerMatrix split_ldd(const std::vector<int> &region_ids){
  
  const size_t num_SNPs=region_ids.size();
  std::array<size_t,1> dims={num_SNPs};
  auto ldres = split_LD(dims,region_ids);
  Rcpp::IntegerMatrix retmat(ldres.size(),3);
  size_t idx=0;
  for(auto ldi = ldres.begin(); ldi!=ldres.end();ldi++){
    retmat(idx,0)=ldi->first;
    auto val= ldi->second;
    retmat(idx,1)=(std::get<0>(val))[0];
    retmat(idx,2)=(std::get<1>(val))[0];
    idx++;
  }
  return(retmat);
}




