#include "sexp_io.hpp"
#include "eigenh5/Selection.hpp"
#include "path.hpp"
#include "utils.hpp"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>
#include <Rcpp.h>
#include <Rinternals.h>


template<int RTYPE> struct r2cpp_t{
  typedef std::false_type type;
};
template<> struct r2cpp_t<INTSXP>{
  typedef int type;
};
template<> struct r2cpp_t<REALSXP>{
  typedef double type;
};
template <> struct r2cpp_t<LGLSXP> { typedef bool type; };
template<> struct r2cpp_t<RAWSXP>{
  typedef Rbyte type;
};
template<> struct r2cpp_t<CHARSXP>{
  typedef std::string type;
};
template<> struct r2cpp_t<STRSXP>{
  typedef std::string type;
};


typedef  std::integral_constant<SEXPTYPE, INTSXP> int_tv;
typedef  std::integral_constant<SEXPTYPE, REALSXP> double_tv;
typedef  std::integral_constant<SEXPTYPE, LGLSXP> bool_tv;
typedef std::integral_constant<SEXPTYPE, STRSXP> string_tv;
typedef std::integral_constant<SEXPTYPE, RAWSXP> raw_tv;
typedef  std::integral_constant<SEXPTYPE,NILSXP> nil_tv;

std::variant<int_tv,double_tv,bool_tv,string_tv,raw_tv> variant_v(SEXPTYPE i){
  switch(i){
  case RAWSXP:
    return raw_tv();
  case INTSXP:
    return int_tv();
  case REALSXP:
    return double_tv();
  case STRSXP:
    return string_tv();
  case LGLSXP:
    return bool_tv();
  default:
    Rcpp::stop("Can't read type");
    return bool_tv();
  }
}




SEXPTYPE h2r_T(const HighFive::DataType htype){

  using namespace HighFive;

  if(htype==HighFive::AtomicType<unsigned char>{})
    return RAWSXP;
  if(htype.same_class(AtomicType<int>()))
    return(INTSXP);
  if(htype.same_class(AtomicType<float>()))
    return(REALSXP);
  if(htype.same_class(AtomicType<bool>()))
    return(LGLSXP);
  if(htype.same_class(AtomicType<std::string>()))
    return(STRSXP);
  Rcpp::Rcerr<<"Couldn't find matching datatype"<<std::endl;
  Rcpp::stop("no matching datatype");
  return(NILSXP);
}


Rcpp::StringVector h2s_T(const HighFive::DataType htype){

  using namespace HighFive;
  if(htype==HighFive::AtomicType<unsigned char>{})
    return("raw");
  if(htype.same_class(AtomicType<int>()))
    return("integer");
  if(htype.same_class(AtomicType<float>()))
    return("double");
  if(htype.same_class(AtomicType<bool>()))
    return("logical");
  if(htype.same_class(AtomicType<std::string>()))
    return("character");

  return(NA_STRING);
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
  

  using namespace Rcpp;
  if(dims.size()==1){
    std::array<size_t,1> tdims;
    std::copy_n(dims.begin(),1,tdims.begin());
    auto ds = DatasetSelection<1>::ProcessList(options,tdims);

    return(List::create(_["order"]=wrap(ds.sels[0].permutation_order())));
  }else{
    if(dims.size()==2){
      std::array<size_t,2> tdims;
      std::copy_n(dims.begin(),2,tdims.begin());
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






SEXP read_attribute(const HighFive::DataSet &der,const std::string attribute_name){

  auto attr = der.getAttribute(attribute_name);

  std::vector<size_t> attr_dims = attr.getSpace().getDimensions();
  if(attr_dims.size()>1){
    Rcpp::stop("Can currently only read	vector and scalar attributes");
  }
  auto my_t = h2r_T(attr.getDataType());
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
  case RAWSXP: {
    std::vector<unsigned char> retv(attr_dims.front());
    attr.read(retv);
    Rcpp::RawVector ret=Rcpp::wrap(retv);
    return(ret);
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

SEXP read_attribute(const HighFive::Group &der,const std::string attribute_name){

  auto attr = der.getAttribute(attribute_name);

  std::vector<size_t> attr_dims = attr.getSpace().getDimensions();
  if(attr_dims.size()>1){
    Rcpp::stop("Can currently only read	vector and scalar attributes");
  }
  auto my_t = h2r_T(attr.getDataType());
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
  case RAWSXP: {
    std::vector<unsigned char> retv(attr_dims.front());
    attr.read(retv);
    Rcpp::RawVector ret=Rcpp::wrap(retv);
    return(ret);
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
  case RAWSXP: {
    std::vector<Rbyte> tdat = Rcpp::as<std::vector<Rbyte> >(data);
    if(auto t_attr = der.openAttribute(attribute_name)){
      t_attr->write(tdat);
    }else{
      auto attr =	der.createAttribute(attribute_name, DataSpace::From(tdat),HighFive::AtomicType<unsigned char>());
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


int longest_string_size(Rcpp::StringVector input,int min_size=255){

  #ifdef DEBUG
  min_size=0;
  #endif


  using input_elem = decltype(input.begin());
  int ret_size=0;
  if(input.begin()!=input.end()){
    ret_size = std::max_element(input.begin(), input.end(),
                                [](input_elem a, input_elem b) {
                                  return a->size() < b->size();
                                })
                   ->size();
  }
  return (std::max(ret_size, min_size)+1);
}



int longest_string_size(std::vector<std::string> input,int min_size=255){

  #ifdef DEBUG
  min_size=0;
  #endif


  int ret_size=0;
  if(input.begin()!=input.end()){
    ret_size = std::max_element(input.begin(), input.end(),
                                [](std::string& a, std::string& b) {
                                  return a.size() < b.size();
                                })->size();
  }
  return (std::max(ret_size, min_size)+1);
}




HighFive::DataSet create_dataset(HighFive::Group &group,
				 const Path &dataname,
				 const Rcpp::RObject &data,
				 HighFive::DataSpace &space,
				 HighFive::Filter &filter,int min_string_size=255){
  using namespace HighFive;
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<int>(), filter));
  }
  case RAWSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<unsigned char>(), filter));
  }
  case REALSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<double>(), filter));
  }
  case STRSXP: {
    int data_size = longest_string_size(Rcpp::as<Rcpp::StringVector>(data),min_string_size);
    return (group.createDataSet(
        dataname, space, HighFive::AtomicType<std::string>(data_size), filter));
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





SEXPTYPE typeof_h5_dset(const HighFive::DataSet &dset){
  using namespace HighFive;
  return(h2r_T(dset.getDataType()));
}

SEXPTYPE typeof_h5_attr(HighFive::Attribute &attr){
  using namespace HighFive;
  return(h2r_T(attr.getDataType()));
}

std::vector<size_t> dataset_dims(std::string filename,
				 std::string datapath){


  return(HighFive::File(filename,HighFive::File::ReadOnly).getDataSet(root_path(datapath)).getDataDimensions());
}


//[[Rcpp::export]]
SEXP read_vector(std::string filename,
		 std::string datapath,
		 Rcpp::List options){
  using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  auto dims = dset.getDataDimensions();
  std::array<size_t,1> tdims{dims[0]};
  auto datasel = DatasetSelection<1>::ProcessList(options,tdims);

  auto file_sel=datasel.makeSelection(dset);
  auto my_t = typeof_h5_dset(dset);
  Rcpp::RObject ret;
  switch (my_t){
  case INTSXP: {
    ret = read_elem_v_h5<INTSXP>(file_sel,datasel);
    break;
  }
  case REALSXP: {
    ret = read_elem_v_h5<REALSXP>(file_sel,datasel);
    break;
  }
  case RAWSXP: {
    ret = read_elem_v_h5<RAWSXP>(file_sel,datasel);
    break;
  }
  case STRSXP: {
    ret = read_elem_v_h5<STRSXP>(file_sel,datasel);
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
  auto attr_v=list_R_attr(dset);
  for(auto &attr :attr_v){
    ret.attr(attr.substr(2))=read_attribute(dset,attr);
  }
  return ret;
}



//[[Rcpp::export]]
SEXP read_matrix(std::string filename,
		 std::string datapath,
		 const Rcpp::List options){

  using namespace Rcpp;

  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto dp= root_path(datapath);

  auto dset = file.getDataSet(dp);
  auto dims = dset.getDataDimensions();
  if(dims.size()!=2){
    Rcpp::stop("dataset is not a matrix");
  }
  std::array<size_t,2> tdims{dims[0],dims[1]};
  auto datasel = DatasetSelection<2>::ProcessList(options,tdims);


  auto file_sel=datasel.makeSelection(dset);
  auto my_t = typeof_h5_dset(dset);

  switch (my_t){
  case INTSXP: {
    auto ret = read_elem_m_h5<INTSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case RAWSXP: {
    auto ret = read_elem_m_h5<RAWSXP>(file_sel,datasel);
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
                   std::string datapath,
                   const Rcpp::List &options){
  using namespace Rcpp;

  auto dp= root_path(datapath);
  // if(datapath[0]!='/'){
  //   datapath="/"+datapath;
  // }
  bool write_success=false;
  HighFive::File file(filename,HighFive::File::ReadWrite);
  if(auto dset = file.openDataSet(dp)){
    auto dims = dset->getDataDimensions();
    if(dims.size()!=2){
      Rcpp::stop("dataset is not a matrix");
    }
    std::array<size_t,2> tdims{dims[0],dims[1]};
    auto datasel = DatasetSelection<2>::ProcessList(options,tdims);
    auto file_sel=datasel.makeSelection(*dset);
    auto my_t = typeof_h5_dset(*dset);
    switch (my_t){
    case INTSXP: {
      Rcpp::Matrix<INTSXP> wmat(data);
      write_elem_m_h5<INTSXP>(file_sel,datasel,wmat);
      write_success=true;
      break;
    }
    case RAWSXP: {
      Rcpp::Matrix<RAWSXP> wmat(data);
      write_elem_m_h5<RAWSXP>(file_sel,datasel,wmat);
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
      Rcpp::Rcerr<<dp<<" has type that can't be written"<<std::endl;
      Rcpp::stop("Can't read type");
    }
    }
  }
  file.flush();
  return(write_success);
}



bool check_string_fits(Rcpp::StringVector input,const HighFive::DataSet dset){
  auto dt_size = dset.getDataType().n_elem();
  using input_elem = decltype(input.begin());
  int ret_size=0;
  if(input.begin()!=input.end()){
    auto ret_el = std::max_element(input.begin(), input.end(),
                                [](input_elem a, input_elem b) {
                                  return a->size() < b->size();
                                });
    auto lss=ret_el->size()+1;
  if(lss > dt_size){
    Rcpp::Rcerr<<"longest string in dataset is of size:"<<
      lss<<"dataset can only fit up to string size: "<<dt_size<<std::endl;
    auto d_dist = ret_el-input.begin();
    Rcpp::Rcerr<<"longest string in dataset is at position:"<<d_dist<<" first "<<dt_size<<" chars:"<<Rcpp::as<std::string>(*ret_el).substr(0,dt_size)<<std::endl;
    return false;
  }
  }
  return true;
}





//[[Rcpp::export]]
bool update_vector(RObject data,
		   std::string filename,
		   std::string datapath,
		   Rcpp::List options){
  using namespace Rcpp;

  bool write_success=false;
  HighFive::File file(filename,HighFive::File::ReadWrite);

  auto dp = root_path(datapath);


  if( auto dset = file.openDataSet(dp)){
    auto dims = dset->getDataDimensions();
    std::array<size_t,1> tdims;
    std::copy_n(dims.begin(),1,tdims.begin());
    auto datasel = DatasetSelection<1>::ProcessList(options,tdims);
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
    case RAWSXP: {
      Rcpp::Vector<RAWSXP> wvec(data);
      write_elem_v_h5<RAWSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    case STRSXP: {
      Rcpp::Vector<STRSXP> wvec(data);
      if(!check_string_fits(wvec,*dset)){
        Rcpp::Rcerr<<"In file"<<filename<<std::endl;
        Rcpp::Rcerr<<"In dataset"<<datapath<<std::endl;
        Rcpp::stop("string will not fit in dataset");
        
      };
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
  const std::map<std::string, hid_t> filters{{"blosc", filter_blosc},
					     {"no_filter", filter_no_filter},
					     {"none", filter_no_filter},
					     {"gzip", filter_gzip},
					     {"deflate", filter_gzip},
					     {"lzf", filter_lzf4},
					     {"zstd",filter_zstd}};
  hid_t	filt = filter_zstd;
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



//[[Rcpp::export]]
bool write_attribute_h5(const RObject &data,
			const std::string &filename,
			std::string datapath){


  using namespace HighFive;
  // if(datapath[0]!='/'){
  //   datapath="/"+datapath;
  // }
  auto dp= root_path(datapath);
  //  bool create_success=false;
  HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);

  if(file.isGroup(dp.parent_path())){
    auto p_obj = file.getGroup(dp.parent_path());
    write_attribute<Group>(p_obj,dp.filename(),data);
  }else{
    if(file.isDataSet(dp.parent_path())){
      auto p_obj = file.getDataSet(dp.parent_path());
      write_attribute<DataSet>(p_obj,dp.filename(),data);
    }
    else{
      Rcpp::stop("Attributes can only be written to DataSets or Groups");
    }
  }
  return(true);
}

//[[Rcpp::export]]
SEXP read_R_attribute_h5(const std::string &filename,
		      std::string datapath){
  using namespace HighFive;

  auto dp= root_path(datapath);

  HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);

  if(file.isGroup(dp.parent_path())){
    auto p_obj = file.getGroup(dp.parent_path());
    return(read_attribute(p_obj,"R:"+dp.filename()));
  }else{
    if(file.isDataSet(dp.parent_path())){
      auto p_obj = file.getDataSet(dp.parent_path());
      return(read_attribute(p_obj,"R:"+dp.filename()));
    }
    else{
      Rcpp::Rcerr<<dp.parent_path()<<" Is not a dataset or group"<<std::endl;
      Rcpp::stop("Attributes can only be read from DataSets or Groups");
    }
  }
}





//[[Rcpp::export]]
SEXP read_attribute_h5(const std::string &filename,
		      std::string datapath){
  using namespace HighFive;

  auto dp= root_path(datapath);

  HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);

  if(file.isGroup(dp.parent_path())){
    auto p_obj = file.getGroup(dp.parent_path());
    return(read_attribute(p_obj,dp.filename()));
  }else{
    if(file.isDataSet(dp.parent_path())){
      auto p_obj = file.getDataSet(dp.parent_path());
      return(read_attribute(p_obj,dp.filename()));
    }
    else{
      Rcpp::Rcerr<<dp.parent_path()<<" Is not a dataset or group"<<std::endl;
      Rcpp::stop("Attributes can only be read from DataSets or Groups");
    }
  }
}




//[[Rcpp::export]]
bool create_dataset_h5(const std::string &filename,
		       std::string datapath,
		       const RObject &data,
		       Rcpp::List options){
   using namespace HighFive;

   // if(datapath[0]!='/'){
   //   datapath="/"+datapath;
   // }
   auto dp=root_path(datapath);
   bool create_success=false;
   HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);


   
#ifdef DEBUG
   Rcpp::Rcerr<<"opening/creating group: "<<dp.parent_path()<<std::endl;
#endif
   auto group = file.openGroup(dp.parent_path());
   if(!group){
     group = file.createGroup(dp.parent_path());
   }

#ifdef DEBUG
   Rcpp::Rcerr<<"opening group: "<<dp.parent_path()<<std::endl;
#endif

   const bool store_float= get_list_scalar<bool>(options,"float").value_or(false);

   auto min_string_size = get_list_scalar<int>(options,"min_string_size").value_or(255);
   auto data_d = get_any_list_element<INTSXP>(options,{"dim","dims"});
   auto	max_d =	get_any_list_element<INTSXP>(options,{"max_dim","max_dims"});
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
   if((data.sexp_type()!=STRSXP) && store_float){
     auto dsr = group->createDataSet(dp.filename(), space, HighFive::AtomicType<float>(), filt);
     auto attrn = data.attributeNames();
     for( auto &attr : attrn){
       if(attr != "dim"){
         Rcpp::RObject att_dat = data.attr(attr);
         if(att_dat.sexp_type() != VECSXP)
           write_attribute(dsr,"R:"+attr,att_dat);
       }
     }
     create_success=true;
   }else{
     auto dsr = create_dataset(group.value(),dp.filename(),data,space,filt,min_string_size);
     auto attrn = data.attributeNames();
     for( auto &attr : attrn){
       if(attr != "dim"){
         Rcpp::RObject att_dat = data.attr(attr);
         if(att_dat.sexp_type() != VECSXP)
           write_attribute(dsr,"R:"+attr,att_dat);
       }
     }
   }
   create_success=true; 
   file.flush();  
  return(create_success);
}
