#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]

template<int RTYPE> struct r2cpp_t{
  typedef std::false_type type;
};
template<> struct r2cpp_t<INTSXP>{
  typedef int type;
};
template<> struct r2cpp_t<REALSXP>{
  typedef double type;
};
template<> struct r2cpp_t<LGLSXP>{
  typedef bool type;
};
template<> struct r2cpp_t<CHARSXP>{
  typedef std::string type;
};
template<> struct r2cpp_t<STRSXP>{
  typedef std::string type;
};







  

SEXPTYPE h2r_T(hid_t htype){

  Eigen::Triplet<double,typename Eigen::SparseMatrix<double>::StorageIndex > Trip;
  auto ht = H5Tget_class(htype);
  if(ht==H5T_INTEGER){
    //Rcpp::Rcout<<"int"<<std::endl;
    return(INTSXP);
  }
  if(ht==H5T_FLOAT){
    //Rcpp::Rcout<<"double"<<std::endl;
    return(REALSXP);
  }
  if(ht==H5T_NATIVE_HBOOL){
    //Rcpp::Rcout<<"BOOL"<<std::endl;
    return(LGLSXP);
  }
  if(ht==H5T_STRING){
    //Rcpp::Rcout<<"string"<<std::endl;
    return(STRSXP);
  }
  // Rcpp::Rcout<<"NIL"<<std::endl;

  return(NILSXP);
}

using namespace Rcpp;


namespace impl {

template <int RTYPE>
int len(const Vector<RTYPE>& x)
{
    return static_cast<int>(x.size());
}

}





std::vector<size_t> obj_dim(RObject x){
  if(x.hasAttribute("dim")){
    return(Rcpp::as<std::vector<size_t> >(x.attr("dim")));
  }else{
    std::vector<size_t> ret={static_cast<size_t>(XLENGTH(SEXP(x)))};
    return(ret);
  }
}

// [[Rcpp::export]]
int len(RObject x)
{
    RCPP_RETURN_VECTOR(impl::len, x);
}


//[[Rcpp::export]]
Rcpp::List permutation_order(const Rcpp::List options, Rcpp::IntegerVector dims){
  std::vector<size_t> tdims(dims.size());
  std::copy(dims.begin(),dims.end(),tdims.begin());
  using namespace Rcpp;
  if(dims.size()==1){
    auto ds = DatasetSelection<1>::ProcessList(options,tdims);

    return(List::create(_["order"]=wrap(ds.sels[0].permutation_order())));
  }else{
    if(dims.size()==2){
      auto ds=DatasetSelection<2>::ProcessList(options,tdims);
    return(List::create(_["rows"]=wrap(ds.sels[0].permutation_order()),
			_["cols"]=wrap(ds.sels[1].permutation_order())));
    }else{
      Rcpp::stop("dims must be length 1 or 2");
    }
  }
}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
Matrix<RTYPE> read_elem_m_h5(HighFive::Selection &file_sel,
			     DatasetSelection<2> &mem_sel){

  auto r_size =	file_sel.getDataDimensions();
  if(mem_sel.doTranspose){
    std::reverse(r_size.begin(),r_size.end());
  }
  if(r_size.size()!=2){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot read matrix dataset unless it is rank 2");
  }
  Rcpp::Matrix<RTYPE> retmat(r_size[0],r_size[1]);
  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&retmat(0,0),r_size[0],r_size[1]);
  mem_sel.readEigen(file_sel,mretmat);
  return(retmat);
}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
void write_elem_m_h5(HighFive::Selection &file_sel,
		     DatasetSelection<2> &mem_sel,
		    Rcpp::Matrix<RTYPE> wmat){

  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=2){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot write matrix dataset unless it is rank 2");
  }
  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&wmat(0,0),wmat.nrow(),wmat.ncol());
  mem_sel.writeEigen(file_sel,mretmat);
}



template <typename Derivate>
SEXP read_attribute(const Derivate der,const std::string attribute_name){

  auto attr = der.getAttribute(attribute_name);
  attr.getDataType().getId();
  std::vector<size_t> attr_dims = attr.getSpace().getDimensions();
  if(attr_dims.size()>1){
    Rcpp::stop("Can currently only read	vector and scalar attributes");
  }
  auto my_t = h2r_T(attr.getDataType().getId());
  switch(my_t){
  case INTSXP:{
    std::vector<int> retv(attr_dims.front());
    attr.read(retv);
    return(Rcpp::wrap(retv));
    break;
  }
  case REALSXP: {
    std::vector<double> retv(attr_dims.front());
    attr.read(retv);
    return(Rcpp::wrap(retv));
    break;
  }
  case STRSXP: {
    std::vector<std::string> retv(attr_dims.front());
    attr.read(retv);
    return(Rcpp::wrap(retv));
    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<attribute_name<<" has type that can't be read"<<std::endl;
    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}

template <typename Derivate>
void write_attribute(Derivate &der,const std::string attribute_name,const Rcpp::RObject data){
  using namespace HighFive;
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    std::vector<int> tdat = Rcpp::as<std::vector<int> >(data);
    if(auto t_attr = der.openAttribute(attribute_name)){
      t_attr->write(tdat);
    }else{
      auto attr =	der.createAttribute(attribute_name, DataSpace::From(tdat),HighFive::AtomicType<int>());
      attr.write(tdat);
    }
    break;
  }
  case REALSXP: {
    std::vector<double> tdat = Rcpp::as<std::vector<double> >(data);
    if(auto t_attr = der.openAttribute(attribute_name)){
      t_attr->write(tdat);
    }else{
      auto attr =	der.createAttribute(attribute_name, DataSpace::From(tdat),HighFive::AtomicType<double>());
      attr.write(tdat);
    }
    break;
  }
  case STRSXP: {
    std::vector<std::string> tdat = Rcpp::as<std::vector<std::string> >(data);
    if(auto t_attr = der.openAttribute(attribute_name)){
      t_attr->write(tdat);
    }else{
      auto attr =	der.createAttribute(attribute_name, DataSpace::From(tdat),HighFive::AtomicType<std::string>());
      attr.write(tdat);
    }
    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::stop("Can't create type");
  }
  }
}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
Rcpp::Vector<RTYPE> read_elem_v_h5(HighFive::Selection &file_sel,
				   DatasetSelection<1> &mem_sel){
  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=1){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot read vector dataset unless it is rank 1");
  }
  std::vector<T> retv(r_size[0]);
  //  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&retmat(0,0),r_size[0],r_size[1]);
  mem_sel.readVector<T>(file_sel,retv);
  return(Rcpp::wrap(retv));
}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
void write_elem_v_h5(HighFive::Selection &file_sel,
		     DatasetSelection<1> &mem_sel,
		    Rcpp::Vector<RTYPE> wvec){

  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=1){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot write vector dataset unless it is rank 1");
  }
  mem_sel.writeVector(file_sel,std::move(Rcpp::as<std::vector<T>>(wvec)));
}


HighFive::DataSet create_dataset(HighFive::Group &group,
				 const std::string &dataname,
				 const Rcpp::RObject &data,
				 HighFive::DataSpace &space,
				 HighFive::Filter &filter){
  using namespace HighFive;
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<int>(), filter));
  }
  case REALSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<double>(), filter));
  }
  case STRSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<std::string>(), filter));
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::stop("Can't create type");
  }
  }
}

SEXPTYPE typeof_h5_dset(HighFive::DataSet &dset){
  using namespace HighFive;
  return(h2r_T(dset.getDataType().getId()));
}

SEXPTYPE typeof_h5_attr(HighFive::Attribute &attr){
  using namespace HighFive;
  return(h2r_T(attr.getDataType().getId()));
}


// SEXP read_h5(const Rcpp::List &file_l){

//   auto fn = get_list_scalar<std::string>(file_l,"filename");
//   if(!fn){
//     Rcpp::stop("Cannot find \"filename\" in	input file_l");
//   }
//   auto fm = std::make_unique<FileManager<true> >(Rcpp::wrap(*fn));
//   auto dset = getDataSet<true>(file_l,fm);
//   auto my_t = typeof_h5_dset(dset);
//   auto tdims = dset.getDataDimensions();
//   const size_t tdim_d = tdims.size();
//   if(tdim_d>2){
//     Rcpp::stop("reading datasets with dimension larger than 2 currently not supported");
//   }
//   if(tdim_d==1)
//     auto datasel = DatasetSelection<2>::ProcessList(file_l,tdims);
//   auto file_sel=datasel.makeSelection(dset);
//   auto ret = read_elem_m_h5<INTSXP>(file_sel,datasel);
//   switch (my_t){
//   case INTSXP: {
//     if(tdim_d==1){
//       DataQueue<1,int>(file_l
//     return(group.createDataSet(dataname, space, HighFive::AtomicType<int>(), filter));
//   }
//   case REALSXP: {
//     return(group.createDataSet(dataname, space, HighFive::AtomicType<double>(), filter));
//   }
//   case STRSXP: {
//     return(group.createDataSet(dataname, space, HighFive::AtomicType<std::string>(), filter));
//   }
//   default: {
//     warning(
// 	    "Invalid SEXPTYPE %d.\n",
// 	    my_t
// 	    );
//     Rcpp::stop("Can't create type");
//   }

//   auto datasel = DatasetSelection<2>::ProcessList(subset,dims);
//   if(tdim==1){
//     dset.getDataType()
//       }

std::vector<size_t> dataset_dims(std::string filename,
				 std::string datapath){

  namespace fs = stdx::filesystem;
  fs::path dp=datapath;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(auto grp =file.openGroup(dp.parent_path())){
    if(auto dset = grp->openDataSet(dp.filename())){
      return(dset->getDataDimensions());
    }
  }
  std::vector<size_t> retvec;
  return(retvec);
}



//[[Rcpp::export]]
SEXP read_vector(std::string filename,
		 std::string datapath,
		 Rcpp::List subset){
  using namespace Rcpp;
  namespace fs = stdx::filesystem;
  fs::path dp=datapath;

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto grp = file.getGroup(groupname);
  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();

  auto datasel = DatasetSelection<1>::ProcessList(subset,dims);

  auto file_sel=datasel.makeSelection(dset);
  auto my_t = typeof_h5_dset(dset);
  //  auto my_t = check_dtype(filename,groupname,dataname);


  switch (my_t){
  case INTSXP: {
    auto ret = read_elem_v_h5<INTSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case REALSXP: {
    auto ret = read_elem_v_h5<REALSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case STRSXP: {
    auto ret = read_elem_v_h5<STRSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}



//[[Rcpp::export]]
SEXP read_matrix(std::string filename,
		 std::string datapath,
		 const Rcpp::List subset){

  using namespace Rcpp;
  namespace fs = stdx::filesystem;

  HighFive::File file(filename,HighFive::File::ReadOnly);
  fs::path dp=datapath;
  auto dset = file.getDataSet(datapath);
  auto dims = dset.getDataDimensions();
  auto datasel = DatasetSelection<2>::ProcessList(subset,dims);


  auto file_sel=datasel.makeSelection(dset);
  auto my_t = typeof_h5_dset(dset);

  switch (my_t){
  case INTSXP: {
    auto ret = read_elem_m_h5<INTSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case REALSXP: {
    auto ret = read_elem_m_h5<REALSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d.\n",
      my_t
    );
    Rcpp::Rcerr<<datapath<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}

//[[Rcpp::export]]
bool update_matrix(RObject data,
                   const std::string filename,
                   const std::string datapath,
                   const Rcpp::List &options){
  using namespace Rcpp;
  namespace fs = stdx::filesystem;
  fs::path dp=datapath;
  bool write_success=false;
  HighFive::File file(filename,HighFive::File::ReadWrite);
  if(auto dset = file.openDataSet(datapath)){
    auto dims = dset->getDataDimensions();
    auto datasel = DatasetSelection<2>::ProcessList(options,dims);
    auto file_sel=datasel.makeSelection(*dset);
    auto my_t = typeof_h5_dset(*dset);
    switch (my_t){
    case INTSXP: {
      Rcpp::Matrix<INTSXP> wmat(data);
      write_elem_m_h5<INTSXP>(file_sel,datasel,wmat);
      write_success=true;
      break;
    }
    case REALSXP: {
      Rcpp::Matrix<REALSXP> wmat(data);
      write_elem_m_h5<REALSXP>(file_sel,datasel,wmat);
      write_success=true;
      break;
    }
    default: {
      warning(
	      "Invalid SEXPTYPE %d.\n",
	      my_t
	      );
      Rcpp::Rcerr<<dp.filename()<<" has type that can't be written"<<std::endl;
      Rcpp::stop("Can't read type");
    }
    }
  }
  file.flush();
  return(write_success);
}









//[[Rcpp::export]]
bool update_vector(RObject data,
		   std::string filename,
		   std::string datapath,
		   Rcpp::List options){
  using namespace Rcpp;

  bool write_success=false;
  HighFive::File file(filename,HighFive::File::ReadWrite);

  if( auto dset = file.openDataSet(datapath)){
    auto dims = dset->getDataDimensions();

    auto datasel = DatasetSelection<1>::ProcessList(options,dims);
    auto file_sel=datasel.makeSelection(*dset);
    auto my_t = typeof_h5_dset(*dset);
    switch (my_t){
    case INTSXP: {
      Rcpp::Vector<INTSXP> wvec(data);
      write_elem_v_h5<INTSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    case REALSXP: {
      Rcpp::Vector<REALSXP> wvec(data);
      write_elem_v_h5<REALSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    case STRSXP: {
      Rcpp::Vector<STRSXP> wvec(data);
      write_elem_v_h5<STRSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    default: {
      warning(
	      "Invalid SEXPTYPE %d.\n",
	      my_t
	      );
      Rcpp::Rcerr<<datapath<<" has type that can't be written"<<std::endl;

      Rcpp::stop("Can't read type");
    }

    }
    file.flush();

  }

  if(!write_success){
    Rcpp::stop("write failed!");
  }
  return(write_success);
}

HighFive::Filter create_filter(std::vector<size_t> data_dimensions,
			       Rcpp::List &options){
  using namespace HighFive;
  const std::map<std::string, hid_t> filters{{"blosc", Filter::blosc},
					     {"no_filter", Filter::no_filter},
					     {"none", Filter::no_filter},
					     {"gzip", Filter::gzip},
					     {"deflate", Filter::gzip},
					     {"lzf", Filter::lzf},
					     {"zstd",Filter::zstd}};
  hid_t	filt = Filter::zstd;
  if (auto tfilt = get_list_scalar<std::string>(options,"filter")){
    auto titer = filters.find(*tfilt);
    if(titer==filters.end()){
      Rcpp::stop("No registered filter for filter: "+*tfilt);
    }
    filt = titer->second;
  }
  auto chunksize_d = get_list_element<INTSXP>(options,"chunksizes",false);
  if(!chunksize_d){
    chunksize_d = get_list_element<INTSXP>(options,"chunksize",false);
  }
  std::vector<size_t> chunk_dimensions;
  if(chunksize_d){
    chunk_dimensions = Filter::guess_chunk(Rcpp::as<std::vector<size_t> >(*chunksize_d));
  }else{
    chunk_dimensions = Filter::guess_chunk(data_dimensions);
  }
  auto filter_opts = get_list_element<INTSXP>(options,"filter_options");
  std::vector<unsigned int> comp_opts;
  if(filter_opts){
    comp_opts = Rcpp::as<std::vector<unsigned int> >(*filter_opts);
  }
  return(Filter(chunk_dimensions,filt,comp_opts));
}




// void write_S4_h5(const std::string filename, std::string datapath,const RObject data){
//   auto attribs = data.attributeNames();

//   for(auto &n :attribs){
//     write_vector_h5(

// }

//[[Rcpp::export]]
bool write_attribute_h5(const std::string &filename,
		      std::string datapath,
		      const RObject &data){


  using namespace HighFive;
  namespace fs = stdx::filesystem;
  if(datapath[0]!='/'){
    datapath="/"+datapath;
  }
  fs::path dp=datapath;
  //  bool create_success=false;
  HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);
  auto p_obj = file.getObject(dp.parent_path());
  std::visit([&](auto&& arg) {
	       using T = std::decay_t<decltype(arg)>;
	       if constexpr (std::is_same_v<T, Group>)
			      write_attribute<Group>(arg,dp.filename(),data);
	       else if constexpr(std::is_same_v<T,DataSet>)
				  write_attribute<DataSet>(arg,dp.filename(),data);
	       else
                static_assert(always_false<T>::value, "non-exhaustive visitor!");
	     },p_obj);
  return(true);
}


//[[Rcpp::export]]
SEXP read_attribute_h5(const std::string &filename,
		      std::string datapath){
  using namespace HighFive;
  namespace fs = stdx::filesystem;
  if(datapath[0]!='/'){
    datapath="/"+datapath;
  }
  fs::path dp=datapath;
  bool create_success=false;
  HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);
  auto p_obj = file.getObject(dp.parent_path());
  return(std::visit([&](auto&& arg) -> SEXP {
	       using T = std::decay_t<decltype(arg)>;
	       if constexpr (std::is_same_v<T, Group>)
			      return(read_attribute<Group>(arg,dp.filename()));
	       else if constexpr(std::is_same_v<T,DataSet>)
				  return(read_attribute<DataSet>(arg,dp.filename()));
	       else
		 static_assert(always_false<T>::value, "non-exhaustive visitor!");
		    },p_obj));
}




//[[Rcpp::export]]
bool create_dataset_h5(const std::string &filename,
		       std::string datapath,
		       const RObject &data,
		       Rcpp::List options){
   using namespace HighFive;
   namespace fs = stdx::filesystem;
   if(datapath[0]!='/'){
     datapath="/"+datapath;
   }
   fs::path dp=datapath;
   bool create_success=false;
   HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);
   auto group = file.openGroup(dp.parent_path()).value_or(file.createGroup(dp.parent_path()));

   auto data_d = get_list_element<INTSXP>(options,"dim");
   if(!data_d){
     data_d =get_list_element<INTSXP>(options,"dims");
   }

   auto	max_d =	get_list_element<INTSXP>(options,"max_dim");
   if(!max_d){
     max_d =get_list_element<INTSXP>(options,"max_dims");
     //     Rcpp::Rcerr<<"max_dims: "<<*max_d<<std::endl;
   }else{
     //     Rcerr<<"no max_dims"<<std::endl;
   }
   std::vector<size_t> max_dvec = Rcpp::as<std::vector<size_t> >(max_d.value_or(Rcpp::IntegerVector::create()));
   if(!max_dvec.empty()){
     for(size_t i=0; i<max_dvec.size();i++){
       if(Rcpp::IntegerVector::is_na((*(max_d))(i))){
         max_dvec[i]=H5S_UNLIMITED;
       }
     }
   }
   std::vector<size_t>	dimvec = Rcpp::as<std::vector<size_t> >(data_d.value_or(Rcpp::wrap(obj_dim(data))));
   Rcpp::IntegerVector tvec;
   auto	filt = create_filter(dimvec,options);
   DataSpace space = DataSpace(dimvec,max_dvec);
   create_dataset(group,dp.filename(),data,space,filt);
   create_success=true;
   file.flush();
  return(create_success);
 }

