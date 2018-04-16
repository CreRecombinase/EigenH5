#include "EigenH5.h"



std::vector<dim_sel> DimRange::find_cont(Rcpp::IntegerVector::const_iterator itb,Rcpp::IntegerVector::const_iterator ite){
  using namespace Rcpp;
  using namespace ranges;


  // auto tar=view::transform(ir,[](int i){
  //     return(i-1);
  //   });
  if(!std::is_sorted(itb,ite)){
    Rcpp::stop("subset_indices must be sorted");
  }
  std::vector<dim_sel> sub_ranges;
  const int n_elem = ite-itb;
  sub_ranges.reserve(n_elem/2);
  auto itbb=itb;
  auto it = itb;
  int tot_dist=0;
  while(it!=ite){
    it = std::adjacent_find(itb,ite,[](int i,int j){
      // Rcpp::Rcout<<"i is : "<<i<<std::endl;
      // Rcpp::Rcout<<"j is : "<<j<<std::endl;
	return(std::abs(j-i)!=1);
    });
    int iti = (it==ite ? *(it-1) : *(it))-1;
    //    int itb_pos = itb-itbb;
    int reg_size = it==ite ? it-itb : (it-itb+1);
    sub_ranges.push_back(dim_sel((*itb)-1,iti,tot_dist,tot_dist+reg_size-1));
    //			 piarray{{{*itb,iti}},{{tot_dist,tot_dist+reg_size-1}}});
    if(it!=ite){
      it++;
    }
    tot_dist=tot_dist+reg_size;
    itb=it;
  }
  return(sub_ranges);
}

// DatasetSelection::DatasetSelection(DimRange row_range, DimRange col_range, std::vector<int> dataset_dimensions_){
// }
