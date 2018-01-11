#include <EigenH5.h>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp14)]]


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


//[[Rcpp::export]]
Rcpp::IntegerMatrix split_ldd(const std::vector<int> &region_ids){
  
  const size_t num_SNPs=region_ids.size();
  std::array<size_t,1> dims={num_SNPs};
  auto ldres = split_LD(dims,std::vector<size_t>(region_ids.begin(),region_ids.end()));
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




