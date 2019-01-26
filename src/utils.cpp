#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include <array>

// [[Rcpp::interfaces(r, cpp)]]



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

    if(!dp.is_absolute()){
      Rcpp::stop("datapath_from must be an absolute path(must begin with \'/\' ("+dp.string()+" is not an absolute path)");
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
  bool ret = false;
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
  return(List::create(_["name"]=wrap(filt_pair.first),
		      _["options"]=wrap(filt_pair.second)));
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
  if(!file.exist(Path(groupname))){
    ret = false;
  }
  if(dataname==""){
    ret = true;
  } else {
    ret = file.getGroup(Path(groupname)).exist(PathNode(dataname, false));
  }
  return (ret);
}

//[[Rcpp::export]]
bool isObject(const std::string filename, std::string dataname) {
  bool ret = false;
  HighFive::File file(filename, HighFive::File::ReadOnly);
  ret = file.exist(Path("/" + dataname));
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
  std::string group_path = full_names ? g_path.string() : std::string();
  const size_t num_cols = grp.getNumberObjects();
  Rcpp::StringVector retvec(num_cols);

  for (int i = 0; i < num_cols; i++) {
    std::string tpath = group_path;
    tpath += grp.getObjectName(i).to_name();
    retvec[i] = tpath;
  }

  return (retvec);
}
//[[Rcpp::export]]
Rcpp::StringVector typeof_h5(const std::string &filename,
                   const std::string &datapath){


  using namespace HighFive;

  if(isGroup(filename,datapath)){
    return(Rcpp::StringVector::create("list"));
  }
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto arg = file.getDataSet(Path(datapath));
  SEXPTYPE h2t = h2r_T(arg.getDataType().getId());
  Rcpp::StringVector ret;
  if (h2t == REALSXP){
    ret = Rcpp::StringVector::create("double");
  }else{
    if (h2t == INTSXP){
      ret = Rcpp::StringVector::create("integer");
    }else{
      if(h2t == STRSXP){
	ret = Rcpp::StringVector::create("character");
      }else{
	ret = Rcpp::StringVector::create("NULL");
      }
    }
  }

  return(ret);
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



