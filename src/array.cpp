template <typename T,size_t Dims>
void read_a_h5(boost::multi_array_ref<T,Dims> &tref,
	       HighFive::DataSet &dset,
	       const CompactSelection<Dims> &comp_sel){

  using namespace HighFive;
  auto is_sorted=comp_sel.get_sorted();
  for(int i=0;i<Dims;i++){
    if(!is_sorted[i]){
      Rcpp::Rcerr<<"In dimension: "<<i<<" of array"<<std::endl;
      Rcpp::stop("indices must be sorted for arrays");
    }
  }
  dset.selectEigen(comp_sel.get_offsets(),comp_sel.get_chunksizes(),{}).read(tref);
}





template <typename T,size_t Dims>
void write_a_h5(boost::multi_array_ref<T,Dims> &tref,
		HighFive::DataSet &dset,
		const CompactSelection<Dims> comp){

  using namespace HighFive;
  auto is_sorted=comp_sel.get_sorted();
  for(int i=0;i<Dims;i++){
    if(!is_sorted[i]){
      Rcpp::Rcerr<<"In dimension: "<<i<<" of array"<<std::endl;
      Rcpp::stop("indices must be sorted for arrays");
    }
  }
  dset.selectEigen(comp_sel.get_offsets(),comp_sel.get_chunksizes(),{}).write(tref);
}



template<typename T,size_t Dims> void block_assign_read(boost::multi_array_ref<T,Dims> &retref,
						     boost::multi_array_ref<T,Dims> &tarr,
						     std::array<boost::multi_array_types::index_range,Dims> ranges){
  if constexpr(Dims==1){
      retref[ boost::indices[ ranges[0] ] ] = tarr;
    }else{
    if constexpr( Dims==2){
	retref[ boost::indices[ ranges[0] ][ ranges[1] ] ] = tarr;
      }else{
      if constexpr( Dims==3){
	  retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ] ] = tarr;
	}else{
	if constexpr( Dims==4){
	    retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ][ ranges[3] ] ] = tarr;
	  }else{
	  static_assert(Dims<=4, "Arrays of dimension > 4 not supported");
	}
      }
    }
  }
}

  template<typename T,size_t Dims> void block_assign_write(boost::multi_array_ref<T,Dims> &retref,
						     boost::multi_array_ref<T,Dims> &tarr,
						     std::array<boost::multi_array_types::index_range,Dims> ranges){
  if constexpr(Dims==1){
      tarr =retref[ boost::indices[ ranges[0] ] ] = ;
    }else{
    if constexpr( Dims==2){
        tarr = retref[ boost::indices[ ranges[0] ][ ranges[1] ] ] ;
      }else{
      if constexpr( Dims==3){
	  tarr =  retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ] ];
	}else{
	if constexpr( Dims==4){
	    tarr = retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ][ ranges[3] ] ] ;
	  }else{
	  static_assert(Dims<=4, "Arrays of dimension > 4 not supported");
	}
      }
    }
  }
}




template <SEXPTYPE RTYPE ,size_t Dims,typename T= typename r2cpp_t<RTYPE>::type,typename RR>
void write_elem_a_h5(T& adata,HighFive::DataSet &dset,const data_selection dsel){

  using namespace ranges;

  const int elem_total= std::accumulate(dsel.n_elem.begin(),dsel.n_elem.end(),1,std::multiplies<int>());
  auto so =  boost::fortran_storage_order();
  boost::multi_array_ref<T,Dims> retref(adata,n_elem,so);
  std::vector<T> tvec;
  const long long N = dsel.num_sel;
  for( long long n=0 ; n<N ; ++n ) {
    CompactSelection<Dims> temp_sel(dsel.cartesian_index(n));
    tvec.resize(temp_sel.get_total_size);
    boost::multi_array_ref<T,Dims> tretref(tvec.data(),temp_sel.get_chunksizes);
    block_assign_write(retref,tretref,temp_sel.get_ranges());
    write_a_h5<T,Dims>(tretref,dset,temp_sel);
  }
}

template <SEXPTYPE RTYPE ,size_t Dims,typename T= typename r2cpp_t<RTYPE>::type,typename RR>
Rcpp::Vector<RTYPE> read_elem_a_h5(HighFive::DataSet &dset,const data_selection<Dims> dsel){

  using namespace ranges;

  const int elem_total= std::accumulate(dsel.n_elem.begin(),dsel.n_elem.end(),1,std::multiplies<int>());
  std::vector<T> rretmat(elem_total);
  auto so =  boost::fortran_storage_order();
  boost::multi_array_ref<T,Dims> retref(&rretmat[0],n_elem,so);
  std::vector<T> tvec;
  const long long N = dsel.num_sel;
  for( long long n=0 ; n<N ; ++n ) {
    CompactSelection<Dims> temp_sel(dsel.cartesian_index(n));
    tvec.resize(temp_sel.get_total_size);
    boost::multi_array_ref<T,Dims> tretref(tvec.data(),temp_sel.get_chunksizes);
    read_a_h5<T,Dims>(tretref,dset,temp_sel);
    block_assign_read(retref,tretref,temp_sel.get_ranges());
  }
  Rcpp::Vector<RTYPE> retmat(rretmat.begin(),rretmat.end());
  retmat.attr("dim")=Rcpp::wrap(std::vector<int>(n_elem.begin(),n_elem.end()));
  return(retmat);
}


//[[Rcpp::export]]
SEXP read_array_h5(const std::string &filename,
                   const std::string &groupname,
                   const std::string &dataname,
		   const Rcpp::List subset_indices = Rcpp::List::create()){
  using namespace Rcpp;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);
  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();
  const int num_dims=dims.size();
  int n_subsets = subset_indices.size();
  if(n_subsets>num_dims){
    Rcpp::Rcerr<<"Rank of "<<groupname<<"/"<<dataname<<" is"<<num_dims<<std::endl;
    Rcpp::Rcerr<<"Rank of selection is is"<<n_subsets<<std::endl;
  }
  if(num_dims>4){
    Rcpp::stop("EigenH5 currently can't handle data with dimensions greater than 4");
  }
  std::vector<std::vector<int> > local_subset_vec(num_dims);
  std::vector<bool> read_subset(num_dims,false);
  std::vector<std::vector<dim_sel> > chunk_vec(num_dims);
  for(int i=0;i<num_dims;i++){
    if(i<n_subsets){
      local_subset_vec[i]=as<std::vector<int> >(as<IntegerVector>(subset_indices[i]));
    }else{
      local_subset_vec[i]={};
    }
    chunk_vec[i] = find_cont(local_subset_vec[i].begin(),local_subset_vec[i].end(),dims[i]);
  }

  auto my_t = check_dtype(filename,groupname,dataname);

  //  std::reverse(local_chunksizes.begin(),local_chunksizes.end());
  switch (my_t){
  case INTSXP: {
    if(num_dims==1){
      data_selection<1> ds(chunk_vec,dims);
      return(read_elem_a_h5<INTSXP,1>(dset,ds));
    }
    if(num_dims==2){
      data_selection<2> ds(chunk_vec,dims);
      return(read_elem_a_h5<INTSXP,2>(dset,ds));
    }
    if(num_dims==3){
      data_selection<3> ds(chunk_vec,dims);
      return(read_elem_a_h5<INTSXP,3>(dset,ds));
    }
    if(num_dims==4){
      data_selection<4> ds(chunk_vec,dims);
      return(read_elem_a_h5<INTSXP,4>(dset,ds));
    }
    break;
  }
  case REALSXP: {
    if(num_dims==1){
      data_selection<1> ds(chunk_vec,dims);
      return(read_elem_a_h5<REALSXP,1>(dset,ds));
    }
    if(num_dims==2){
      data_selection<2> ds(chunk_vec,dims);
      return(read_elem_a_h5<REALSXP,2>(dset,ds));
    }
    if(num_dims==3){
      data_selection<3> ds(chunk_vec,dims);
      return(read_elem_a_h5<REALSXP,3>(dset,ds));
    }
    if(num_dims==4){
      data_selection<4> ds(chunk_vec,dims);
      return(read_elem_a_h5<REALSXP,4>(dset,ds));
    }
    break;
  }
  case STRSXP: {
    if(num_dims==1){
      data_selection<1> ds(chunk_vec,dims);
      return(read_elem_a_h5<STRSXP,1>(dset,ds));
    }
    if(num_dims==2){
      data_selection<2> ds(chunk_vec,dims);
      return(read_elem_a_h5<STRSXP,2>(dset,ds));
    }
    if(num_dims==3){
      data_selection<3> ds(chunk_vec,dims);
      return(read_elem_a_h5<STRSXP,3>(dset,ds));
    }
    if(num_dims==4){
      data_selection<4> ds(chunk_vec,dims);
      return(read_elem_a_h5<STRSXP,4>(dset,ds));
    }
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
void write_array_h5(const std::string &filename,
                   const std::string &groupname,
                   const std::string &dataname,
		    Rcpp::RObject data,
		    const Rcpp::List subset_indices = Rcpp::List::create(),
		    const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()){
  using namespace Rcpp;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.createOrGetGroups(groupname);
  const bool create_ds=!grp.exist(dataname);

  if(create_ds){
    auto my_t = data.sexp_type();
    std::vector<int> dtdims =as<std::vector<size_t> >(data.attr("dim"));
    std::vector<size_t> ddims(dtdims.begin(),dtdims.end());
    
    switch(my_t){
    case INTSXP:{
      create_m_h5<int>(ddims,grp,dataname,false,as<std::vector<size_t> >(chunksizes));
      break;
    }
    case REALSXP:{
      create_m_h5<double>(ddims,grp,dataname,false,as<std::vector<size_t> >(chunksizes));
      break;
    }
    case STRSXP:{
      create_m_h5<std::string>(ddims,grp,dataname,false,as<std::vector<size_t> >(chunksizes));
      break;
    }
    default: {
      warning(
	      "Invalid SEXPTYPE %d.\n",
	      my_t
	      );
      Rcpp::stop("Unable to write data of this type");
    }
    }
  }
  auto dset = grp.getDataSet(dataname);
  auto dims = dset.getDataDimensions();
  const int num_dims=dims.size();
  int n_subsets = subset_indices.size();
  if(n_subsets>num_dims){
    Rcpp::Rcerr<<"Rank of "<<groupname<<"/"<<dataname<<" is"<<num_dims<<std::endl;
    Rcpp::Rcerr<<"Rank of selection is is"<<n_subsets<<std::endl;
  }
  if(num_dims>4){
    Rcpp::stop("EigenH5 currently can't handle data with dimensions greater than 4");
  }
  std::vector<std::vector<int> > local_subset_vec(num_dims);
  std::vector<bool> read_subset(num_dims,false);
  std::vector<std::vector<dim_sel> > chunk_vec(num_dims);
  for(int i=0;i<num_dims;i++){
    if(i<n_subsets){
      local_subset_vec[i]=as<std::vector<int> >(as<IntegerVector>(subset_indices[i]));
    }else{
      local_subset_vec[i]={};
    }
    chunk_vec[i] = find_cont(local_subset_vec[i].begin(),local_subset_vec[i].end(),dims[i]);
  }

  //  auto my_t = check_dtype(filename,groupname,dataname);

  //  std::reverse(local_chunksizes.begin(),local_chunksizes.end());
  switch (my_t){
  case INTSXP: {
    Rcpp::Vector<INTSXP> td(dtata);

    if(num_dims==1){
      data_selection<1> ds(chunk_vec,dims);
      return(write_elem_a_h5<INTSXP,1>(&td[0],dset,ds));
    }
    if(num_dims==2){
      data_selection<2> ds(chunk_vec,dims);
      return(write_elem_a_h5<INTSXP,2>(&td[0],dset,ds));
    }
    if(num_dims==3){
      data_selection<3> ds(chunk_vec,dims);
      return(write_elem_a_h5<INTSXP,3>(&td[0],dset,ds));
    }
    if(num_dims==4){
      data_selection<4> ds(chunk_vec,dims);
      return(write_elem_a_h5<INTSXP,4>(&td[0],dset,ds));
    }
    break;
  }
  case REALSXP: {
    Rcpp::Vector<INTSXP> td(dtata);
    if(num_dims==1){
      data_selection<1> ds(chunk_vec,dims);
      return(write_elem_a_h5<REALSXP,1>(&td[0],dset,ds));
    }
    if(num_dims==2){
      data_selection<2> ds(chunk_vec,dims);
      return(write_elem_a_h5<REALSXP,2>(&td[0],dset,ds));
    }
    if(num_dims==3){
      data_selection<3> ds(chunk_vec,dims);
      return(write_elem_a_h5<REALSXP,3>(&td[0],dset,ds));
    }
    if(num_dims==4){
      data_selection<4> ds(chunk_vec,dims);
      return(write_elem_a_h5<REALSXP,4>(&td[0],dset,ds));
    }
    break;
  }
  case STRSXP: {
    Rcpp::Vector<STRSXP> td(dtata);
    std::vector<std::string> ntd=as::<std::vector<std::string> >td;
    if(num_dims==1){
      data_selection<1> ds(chunk_vec,dims);
      return(write_elem_a_h5<STRSXP,1>(&ntd[0],dset,ds));
    }
    if(num_dims==2){
      data_selection<2> ds(chunk_vec,dims);
      return(write_elem_a_h5<STRSXP,2>(&ntd[0],dset,ds));
    }
    if(num_dims==3){
      data_selection<3> ds(chunk_vec,dims);
      return(write_elem_a_h5<STRSXP,3>(&ntd[0],dset,ds));
    }
    if(num_dims==4){
      data_selection<4> ds(chunk_vec,dims);
      return(write_elem_a_h5<STRSXP,4>(&ntd[0],dset,ds));
    }
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