#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[depends(RcppParallel)]]
//[[Rcpp::plugins(cpp17)]]
#include <array>
// [[Rcpp::interfaces(r, cpp)]]
#include "rutils/rutils.hpp"
#if __has_include(<charconv>)
#include <charconv>
#else
#include <cstdio>
#endif
#include <set>
#include <iostream>
#include <algorithm>




std::unordered_map<int,int> reverse_index(Rcpp::IntegerVector index_a){
    const size_t p_a=index_a.size();
    std::unordered_map<int,int> ind_a(p_a);
    auto iap = ind_a.begin();
    for(int i=0; i<p_a; i++){
      iap = ind_a.emplace_hint(iap,index_a(i),i);
    }
    return ind_a;
}

//[[Rcpp::export]]
Rcpp::List join_ids(Rcpp::IntegerVector index_a,Rcpp::IntegerVector index_b){
  const size_t p_a=index_a.size();
  const size_t p_b=index_b.size();

  const auto map_index_a=reverse_index(index_a);
  std::vector<int> sub_ia;
  sub_ia.reserve(std::min(p_a,p_b));
  std::vector<int> sub_ib;
  sub_ib.reserve(std::min(p_a,p_b));

  for(int i=0; i<p_b; i++){
    auto ipm = map_index_a.find(index_b(i));
    if(ipm != map_index_a.end()){
      sub_ia.push_back(ipm->second);
      sub_ib.push_back(i);
    }
  }
  using namespace Rcpp;

  const auto ns = sub_ia.size();
  auto m_ret_l = List::create(_["idx_a"]=Rcpp::wrap(sub_ia),
                              _["idx_b"]=Rcpp::wrap(sub_ib));
  m_ret_l.attr("class") = StringVector::create("tbl_df","tbl","data.frame");
  m_ret_l.attr("row.names") = seq(1,ns);

  return(m_ret_l);


}


//[[Rcpp::export]]
Rcpp::IntegerVector fast_str2int(Rcpp::StringVector input,const int offset=0){

  SEXP str_sxp(input);
  const size_t p =input.size();
  Rcpp::IntegerVector ret(p);
  auto pp = get_string_ptr(str_sxp);
  int tresult;
  std::transform(pp,pp+p,ret.begin(),[&](SEXP x){
                                       if(x==R_NaString)
                                         return NA_INTEGER;
                                       const size_t strl=LENGTH(x);
                                       const char* chp = CHAR(x);
                                       const char* beg=chp+offset;
                                       const char* end=chp+LENGTH( x );
                                       if(offset>=strl){
                                         Rcpp::Rcerr<<"For string"<<CHAR(x)<<std::endl;
                                         Rcpp::Rcerr<<"LEN(x): "<<LENGTH(x)<<std::endl;
                                         Rcpp::Rcerr<<"string offset greater than string length"<<std::endl;
                                         return NA_INTEGER;
                                       }

#if __has_include(<charconv>)
                                       if(auto [p, ec] = std::from_chars(beg, end, tresult);
                                          ec == std::errc())
                                         return tresult;
#else
                                       if(sscanf (beg,"%d",&tresult)>0)
                                         return(tresult);
#endif
                                       return (NA_INTEGER);


                                     });
  return(ret);
}


//[[Rcpp::export]]
Rcpp::IntegerVector fast_str2ascii(Rcpp::StringVector input,int offset=0){

  SEXP str_sxp(input);
  const size_t p =input.size();
  Rcpp::IntegerVector ret(p);
  auto pp = get_string_ptr(str_sxp);
  std::transform(pp,pp+p,ret.begin(),[&](SEXP x){
                                       if(x==R_NaString)
                                         return NA_INTEGER;
                                       const size_t strl=LENGTH(x);
                                       const char* chp = CHAR(x);
                                       const char* beg=chp+offset;
                                       const char* end=chp+LENGTH( x );
                                       if(offset>=strl){
                                         Rcpp::Rcerr<<"For string"<<CHAR(x)<<std::endl;
                                         Rcpp::Rcerr<<"LEN(x): "<<LENGTH(x)<<std::endl;
                                         Rcpp::Rcerr<<"string offset greater than string length";
                                         return(NA_INTEGER);
                                       }

                                       return static_cast<int>(*beg);
                                     });
  return(ret);
}


//[[Rcpp::export]]
void link_objects_h5(Rcpp::StringVector filename_from ,const std::string filename_to, Rcpp::StringVector datapath_from, Rcpp::StringVector datapath_to){

  auto to_file=HighFive::File(filename_to,HighFive::File::ReadWrite|HighFive::File::Create);



  const size_t num_files=datapath_from.size();
  if(num_files!=filename_from.size()){
    Rcpp::stop("filename_from must be same length as datapath_from");
  }
  if(num_files !=datapath_to.size()){
    Rcpp::stop("datapath_to must be same length as datapath_from");
  }
  for(int i=0 ;i<num_files;i++){
    const auto dp_to=CHAR(STRING_ELT(datapath_to,i));
    const auto fn_from=CHAR(STRING_ELT(filename_from,i));
    Path dp(std::string(CHAR(STRING_ELT(datapath_from,i))));

    if(dp.nodes.front()!='/'){
      Rcpp::stop("datapath_from must be an absolute path(must begin with \'/\' ("+dp+" is not an absolute path)");
    }
    H5Lcreate_external(fn_from, dp.c_str(), to_file.getId(), dp_to, (hid_t)0, (hid_t)0);
  }
}

//[[Rcpp::export]]
void create_file_h5(const std::string filename){
  HighFive::File file(filename,HighFive::File::Create);
}


//[[Rcpp::export]]
Rcpp::IntegerVector dataset_chunks(const std::string filename,
				   const std::string datapath){
  using namespace HighFive;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  return(Rcpp::wrap(file.getDataSet(Path(datapath)).getFilter().get_chunksizes()));
}


//[[Rcpp::export]]
void extend_dataset(const std::string filename,
		    const std::string datapath,
		    Rcpp::IntegerVector newdims){
  using namespace HighFive;
  HighFive::File file(filename,HighFive::File::ReadWrite);
  file.getDataSet(Path(datapath)).extend(Rcpp::as<std::vector<size_t> >(newdims));
}


//[[Rcpp::export]]
void extend_dataset_by(const std::string filename,
		    const std::string datapath,
		       Rcpp::IntegerVector newdims){
  using namespace HighFive;
  HighFive::File file(filename,HighFive::File::ReadWrite);
  auto ds = file.getDataSet(Path(datapath));
  Rcpp::IntegerVector cdim =Rcpp::wrap(ds.getDataDimensions());
  cdim=cdim+newdims;
  ds.extend(Rcpp::as<std::vector<size_t>>(cdim));
}


//[[Rcpp::export]]
Rcpp::List get_datset_filter(const std::string filename, const std::string datapath){
  using namespace HighFive;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto filt_pair = file.getDataSet(Path(datapath)).getFilter().get_filter_info();
  using namespace Rcpp;
  return wrap_pair(filt_pair,"name","options");
  // return(List::create(_["name"]=wrap(filt_pair.first),
  //       	      _["options"]=wrap(filt_pair.second)));
}


//[[Rcpp::export]]
Rcpp::IntegerVector guess_chunks(const std::vector<int> dimsize){
  std::vector<size_t> tdims(dimsize.begin(),dimsize.end());
  return(Rcpp::wrap(HighFive::Filter::guess_chunk(tdims)));
}


//[[Rcpp::export]]
bool exists_h5(const std::string filename,
	       const std::string groupname="/",
	       const std::string dataname=""){
  using namespace HighFive;
  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(!file.getGroup(Path("/")).exist(Path(groupname))){
    ret = false;
  }
  if(dataname==""){
    ret = true;
  } else {
    ret = file.getGroup(Path(groupname)).exist(Path(dataname));
  }
  return (ret);
}

//[[Rcpp::export]]
bool isObject(const std::string filename, std::string dataname) {
  bool ret = false;
  HighFive::File file(filename, HighFive::File::ReadOnly);
  ret = file.getGroup(Path("/")).exist(Path("/" + dataname));
  return (ret);
}


//[[Rcpp::export]]
int ArrayTypeSize(const std::string filename, std::string dataname) {

  auto ret = HighFive::File(filename,HighFive::File::ReadOnly).getGroup(Path("/")).getDataSet(Path("/" + dataname)).getDataType().n_elem();
  return (ret);
}


//[[Rcpp::export]]
bool isDataSet(const std::string filename, std::string dataname){

  if(dataname[0]!='/'){
    dataname="/"+dataname;
  }
  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(file.exist(Path(dataname))){
    ret = file.isDataSet(Path(dataname));
    return(ret);
  } else {
    Rcpp::stop("Object does not exist!");
  }
  Rcpp::stop("unable to open file for reading!");
}

//[[Rcpp::export]]
bool isGroup(const std::string filename, std::string dataname){


  if(dataname[0]!='/'){
    dataname="/"+dataname;
  }
  Path d_path(dataname);

  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(file.exist(d_path)){
    ret = file.isGroup(d_path);
  }else{
    Rcpp::stop("Object does not exist!");
  }
  return(ret);
}



//[[Rcpp::export("ls_h5_exp")]]
Rcpp::StringVector ls_h5(const std::string filename,Rcpp::CharacterVector groupname = Rcpp::CharacterVector::create("/"),
                         bool full_names=false){

  HighFive::File file(filename,HighFive::File::ReadOnly);
  HighFive::Group grp;

  std::string tgroupname = Rcpp::as<std::string>(groupname(0));
  if (tgroupname.back() != '/') {
    tgroupname += "/";
  }
  //stdx::filesystem::path d_path = dataname;
  Path g_path(tgroupname);

  //HDF5ErrMapper::ToException<GroupException>(
  grp = file.getGroup(g_path);
  std::string group_path = full_names ? g_path.nodes : std::string();
  const size_t num_cols = grp.getNumberObjects();
  Rcpp::StringVector retvec(num_cols);

  for (int i = 0; i < num_cols; i++) {
    std::string tpath = group_path;
    tpath += grp.getObjectName(i);
    retvec[i] = tpath;
  }

  return (retvec);
}





//[[Rcpp::export]]
Rcpp::StringVector typeof_h5(const std::string filename,
                   const std::string datapath){


  using namespace HighFive;

  if(isGroup(filename,datapath)){
    return(Rcpp::StringVector::create("list"));
  }
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto arg = file.getDataSet(Path(datapath));
  return(h2s_T(arg.getDataType()));
}

//[[Rcpp::export]]
Rcpp::List info_h5(const Rcpp::StringVector filename, Rcpp::StringVector datapaths){
  using namespace Rcpp;
  const auto file = HighFive::File(as<std::string>(filename[0]),HighFive::File::ReadOnly);

  const size_t num_datasets = datapaths.size();
  std::vector<HighFive::DataSet> dsets;

  List dim_l(num_datasets);
  StringVector types(num_datasets);
  IntegerVector disk_size(num_datasets);
  List filter_l(num_datasets);

  //  std::unordered_set<size_t> ds_sizes;
  for(int i=0; i<num_datasets;i++){
    auto obj_v = file.getObject(as<std::string>(datapaths(i)));
    if(auto dset = std::get_if<HighFive::DataSet>(&obj_v)){
      auto dsd = dset->getSpace().getDimensions();
      IntegerVector tr(dsd.begin(),dsd.end());
      dim_l[i]=tr;
      types[i]=h2s_T(dset->getDataType())[0];
      disk_size[i] = static_cast<int>(dset->getStorageSize());
      filter_l[i]=wrap_pair(dset->getFilter().get_filter_info(),"name","options");
    }else{
      dim_l[i]=IntegerVector::create(NA_INTEGER);
      types[i]="group";
      disk_size[i]=NA_INTEGER;
      filter_l[i]=NA_INTEGER;
    }
  }
  auto m_ret_l = List::create(_["name"]=datapaths,
                          _["type"]=types,
                          _["storage_size"]=disk_size,
                          _["dims"]=dim_l,
                          _["filter"]=filter_l);
  m_ret_l.attr("class") = StringVector::create("tbl_df","tbl","data.frame");
  m_ret_l.attr("row.names") = seq(1, num_datasets);
  return m_ret_l;
}










// 
// Rcpp::IntegerVector subset_h5(const std::string	filename, Rcpp::DataFrame check,const std::string path_prefix="/",const int chunksize=5000){
// 
//   using namespace HighFive;
// 
// 
//   HighFive::File file(filename,HighFive::File::ReadOnly);
// 
//   auto group=file.getGroup(path_prefix);
//   auto test_names = df.attr("names");
//   std::map<std::string,DataSet> ds;
//   size_t op=0;
//   for(auto it : test_names){
//     if( auto dsn = group.openDataSet(it)){
//       auto p_d = dsn->getDataDimensions();
//       if(p_d.size()!=1){
// 	if(p_d.[1]!=1){
// 	  Rcpp::Rcerr<<"In datapath"<<path_prefix<<"/"<<it<<std::endl;
// 	  Rcpp::Rcerr<<"dataset is of dimensions:";
// 	  for( auto ip : p_d){
// 	    Rcpp::Rcerr<<ip<<"\n";
// 	  }
// 
// 	  Rcpp::stop("Dataset must be vector or one column matrix");
// 	}
//       }
//       if(op=0){
// 	op=p_d[0];
//       }
//       if(p_d[0]!=op){
// 	Rcpp::Rcerr<<"In datapath"<<path_prefix<<"/"<<it<<std::endl;
// 	Rcpp::Rcerr<<"data is of length "<<p_d[0]<<std::endl;
// 	Rcpp::stop("datasets must all be of the same dimension!");
//       }
//       ds.insert(it,*dsn);
//     }
//   }
// 
// 
// }


inline bool exists_file (const std::string& name) {
    std::ifstream f(name.c_str());
    return f.good();
}

//[[Rcpp::export]]
Rcpp::DataFrame file_acc_ct(const std::string filename){
  using namespace Rcpp;
  if(exists_file(filename)){
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto ret = Rcpp::DataFrame::create(
      _["count"]=Rcpp::IntegerVector::create(
        file.getObjCount(H5F_OBJ_FILE)-1,
        file.getObjCount(H5F_OBJ_DATASET),
        file.getObjCount(H5F_OBJ_GROUP),
        file.getObjCount(H5F_OBJ_DATATYPE),
        file.getObjCount(H5F_OBJ_ATTR)),
        _["type"]=Rcpp::StringVector::create(
          "Files",
          "DataSets",
          "Groups",
          "DataTypes",
          "Attributes"
        ));

  return(ret);
  }else{
    return(Rcpp::DataFrame::create(
      _["count"]=Rcpp::IntegerVector::create(0,
					     0,
					     0,
					     0,
					     0),
      _["type"]=Rcpp::StringVector::create(
					   "Files",
					   "DataSets",
					   "Groups",
					   "DataTypes",
					   "Attributes"
					   )));

  }
}



//[[Rcpp::export]]
Rcpp::IntegerVector dim_h5(const std::string &filename,const std::string datapath){

  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto ret =Rcpp::wrap(file.getDataSet(Path(datapath)).getDataDimensions());
  return(ret);
}


//[[Rcpp::export]]
void concat_mats(const std::string newfile, const std::string newpath, Rcpp::List selections, int margin=0){

  FileManager<false> fm;
  DataQueue<2,double,false> dq(selections,fm);
  using namespace HighFive;
  const size_t num_sel=dq.getNumSelections();
  Rcpp::Rcout<<"There are "<<num_sel<<" selections";
  std::vector<std::vector<size_t> > source_dim(num_sel);
  std::vector<size_t> tot_dim;

  for(int i=0; i<num_sel;i++){
    if(i==0){
      tot_dim= dq.get_index_selection(i).getDataDimensions();
    }else{
      tot_dim[margin]+=dq.get_index_selection(i).getDataDimensions()[margin];
    }
  }
  //  Rcpp::Rcout<<"total_dim is: "<<tot_dim[0]<<"x"<<tot_dim[1]<<std::endl;
  auto dcpl = H5Pcreate(H5P_DATASET_CREATE);
  auto virt_space = DataSpace(tot_dim);
  std::vector<size_t> offset(tot_dim.size(),0);

  for(int i=0; i<num_sel;i++){
    auto msel = dq.get_index_selection(i);
    auto msel_dim=dq.get_index_selection(i).getDataDimensions();
    auto fn =	get_list_scalar<std::string>(selections(i),"filename");
    auto dn =	get_list_scalar<std::string>(selections(i),"datapath");
    if(!(fn && dn)){
      Rcpp::stop("filename + datapath must be specified for each list element in selections");
    }
    std::vector<hsize_t> offset_local(offset.size());
    std::vector<hsize_t> count_local(msel_dim.size());
    std::copy(offset.begin(),offset.end(),offset_local.begin());
    std::copy(msel_dim.begin(),msel_dim.end(),count_local.begin());
    H5Sselect_hyperslab(virt_space.getId(),H5S_SELECT_SET,offset_local.data(),NULL,count_local.data(),NULL);
    H5Pset_virtual(dcpl,virt_space.getId(),fn->c_str(),dn->c_str(),msel.getMemSpace().getId());
    offset[margin]+=msel_dim[margin];
  }

  auto maf= fm.get_file(newfile);
  auto dset = H5Dcreate2(maf.getId(),
			 newpath.c_str(),
			 AtomicType<double>().getId(),
			 virt_space.getId(),
			 H5P_DEFAULT,
			 dcpl,H5P_DEFAULT);
  H5Dclose(dset);

}
