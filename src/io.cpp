// #include <boost/iostreams/stream_buffer.hpp>
// #include <boost/iostreams/stream.hpp>
// #include <boost/iostreams/filtering_stream.hpp>
// #include <boost/iostreams/filter/gzip.hpp>
// #include <boost/iostreams/device/mapped_file.hpp>
// #include <boost/utility/string_ref.hpp>
// #include <boost/spirit/include/qi.hpp>
//#include <charconv>
//#include <gsl/span>
#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]



// template<typename string_type>
// inline bool str_to_value(const string_type& src, double& dest)
// {
//     namespace qi = boost::spirit::qi;

//     return qi::parse(std::cbegin(src), std::cend(src), qi::double_, dest);
// }

// template<typename string_type>
// inline bool str_to_value(const string_type& src, int& dest)
// {
//     namespace qi = boost::spirit::qi;

//     return qi::parse(std::cbegin(src), std::cend(src), qi::int_, dest);
// }

// struct data_buff{
//   std::string buffer;
//   std::pair<int,int> pos;
//   std::string_view tbuff;
// };


// class mach_file{
//   boost::iostreams::filtering_istream &fs;
//   const std::vector<std::string> &sample_names;
//   const size_t num_rows;
//   const std::vector<int> &snp_indices;
//   const size_t snp_ind_size;
//   const size_t p;
//   const size_t max_buffer_size;
//   const bool SNPfirst;
//   //  const std::vector<size_t> line_sizes;
//   HighFive::DataSet & ds;
//   Progress prog_bar;
//   std::string region_buffer;
//   std::string current_sample;
//   //  size_t line_pos; //line cursor (should always be inside the buffer)
//   size_t snp_idx; //Which snp index am I on?
//   size_t line_no; // Which sample am I on?
//   std::pair<int,int> buffer_pos; //Which byte in the buffer am I on (start and one past end)?
//   size_t sample_offset;
//   std::vector<double> data;
// public:
//   mach_file(boost::iostreams::filtering_istream	&fs_,
// 	    const std::vector<std::string> &sample_names_,
// 	    const std::vector<int> &snp_indices_, const size_t p_,
// 	    const size_t buffer_size_,HighFive::DataSet &ds_,const bool SNPfirst_=true,const bool progress=true):
//     fs(fs_),
//     sample_names(sample_names_),
//     num_rows(sample_names.size()),
//     snp_indices(snp_indices_),
//     snp_ind_size(snp_indices.size() ),
//     p(p_),
//     max_buffer_size(buffer_size_),
//     ds(ds_),
//     SNPfirst(SNPfirst_),
//     prog_bar(num_rows,progress)
//   {
//     line_no=0;
//     region_buffer.reserve(max_buffer_size);
//     data.reserve(max_buffer_size/6);
//     buffer_pos={0,0};
//     snp_idx=0;
//     get_current_sample();
//     //    size_t empty_rsize=name_sizes[0]+6;
//   }
// private:
//   size_t snp_line_pos(const size_t idx) const{
//     const size_t cur_snp=snp_indices[idx];
//     //    const size_t empty_rsize=name_sizes[line_no]+6;
//     return(cur_snp*6);
//   }
//   // size_t snp_abs_pos(const size_t idx, const size_t line_idx)const{
//   //   return((name_sizes[line_idx]+6+p*6)

//   bool scan_until_snp(){
//     const size_t cur_snp_pos = snp_line_pos(snp_idx);
//     if(cur_snp_pos>=buffer_pos.second){
//       fs.ignore(cur_snp_pos-buffer_pos.second);
//       buffer_pos.second=cur_snp_pos;
//       buffer_pos.first=cur_snp_pos;
//       return(true);
//     }else{
//       size_t buff_ahead=cur_snp_pos-buffer_pos.first;
//       region_buffer.substr(buff_ahead,buffer_pos.second-cur_snp_pos);
//       return(false);
//     }
//   }

//   void extend_buffer(){
//     const size_t cur_snp_pos = snp_line_pos(snp_idx);
//     size_t prel_snp=snp_idx;
//     size_t prel_pos=snp_line_pos(prel_snp)+6;
//     while((prel_snp+1)<snp_ind_size){
//       if(((snp_line_pos(prel_snp+1)-cur_snp_pos)<(max_buffer_size))){
// 	prel_snp++;
// 	prel_pos=snp_line_pos(prel_snp)+6;
//       }else{
// 	break;
//       }
//     }

//     region_buffer.resize(prel_pos-cur_snp_pos);
//     fs.read(region_buffer.data(),prel_pos-cur_snp_pos);
//     buffer_pos.second=prel_pos;
//   }
//   void get_current_sample(){
//     std::string	sample_id;
//     std::getline(fs,sample_id,'\t');
//     auto ret = std::find(sample_names.begin(),sample_names.end(),sample_id);
//     if(ret == sample_names.end()){
//       Rcpp::stop("sample_id: "+sample_id+" not found!");
//     }else{
//       sample_offset=ret-sample_names.begin();
//     }
//     std::getline(fs,sample_id,'\t');
//     // fs.read(region_buffer.data(),5);
//     if(sample_id!="DOSE"){
//       Rcpp::stop("Not at the beginning of line_no:"+std::to_string(line_no)+":\n"+sample_id+"\nExpecting:\nDOSE");
//     }
//   }

//   void advance_line(){
//     const size_t line_remaining=p*6-buffer_pos.second;
//     fs.ignore(line_remaining);
//     line_no++;
//     //    region_buffer.resize(name_sizes[line_no]+6);
//     get_current_sample();
//     buffer_pos={0,0};
//     region_buffer.clear();
//     prog_bar.increment();
//     if (Progress::check_abort() ){
//       Rcpp::stop("Process Interrupted!");
//     }
//     snp_idx=0;
//   }

//   bool parse_chunk(){
//     //stuff the buffered SNPs into the data vector until:
//     //1. the data vector is  full (return true)
//     //2. the buffer is empty (return false)
//     // If 1. We'll "artificially" advance the buffer to	the next SNP
//     std::string_view tbuff(region_buffer);
//     size_t buffer_size=buffer_pos.second-buffer_pos.first;
//     size_t n_buffer_snps=buffer_size/6;
//     size_t cur_snp_pos =snp_line_pos(snp_idx);
//     size_t cur_snp=snp_indices[snp_idx];
//     size_t buffer_snp=cur_snp;
//     size_t start_snp=cur_snp;
//     //first snp in the buffer is always cur_snp,
//     //last snp in the buffer is in snp_indices

//     double tres;

//     for(int i=0; i<n_buffer_snps;i++){
//       buffer_snp=start_snp+i;
//       if(buffer_snp == cur_snp){
// 	str_to_value(tbuff,tres);
// 	data.push_back(tres);
// 	buffer_size-=6;
// 	buffer_pos.first+=6;
// 	tbuff=tbuff.substr(6,buffer_size);
// 	if((snp_idx==(snp_ind_size-1)) || (data.capacity()==data.size())){
// 	  write_buffer();
// 	  if(snp_idx==(snp_ind_size-1)){
// 	    if((line_no+1)<num_rows){
// 		advance_line();
// 		return(true);
// 	    }else{
// 	      return(false);
// 	    }
// 	  }
// 	}
// 	snp_idx++;
// 	cur_snp=snp_indices[snp_idx];
//       }else{
// 	buffer_size-=6;
// 	buffer_pos.first+=6;
// 	tbuff=tbuff.substr(6,buffer_size);
//       }
//     }
//     region_buffer=tbuff;
//     return(true);
//   }

//   //  size_t sample_offset(const std::string &sample_id){


//   void write_buffer(){
//     const size_t data_size=data.size();
//     const size_t data_start=(snp_idx>data_size) ? snp_idx-(data_size)+1 : (data_size-snp_idx-1);
//     //    prog_bar.increment(data_size);
//     if(SNPfirst){
//       ds.select({data_start,sample_offset},{data_size,1},{}).write(data);
//     }else{
//       ds.select({sample_offset,data_start},{1,data_size},{}).write(data);
//     }
//     data.clear();
//     //    data_start+=datasize;
//   }
// public:
//   void process_file(const bool progress=true){
//     do{
//       if(scan_until_snp()){
// 	extend_buffer();
//       }
//     }while(parse_chunk());

//   }
// };

// void mach2h5(const std::string dosagefile, const std::string h5file, const std::string datapath,std::vector<int> snp_idx, std::vector<std::string> names, const int p,Rcpp::List options){

//   const size_t num_elem=names.size();
//   using namespace HighFive;
//   size_t mp=p;
//   File file(h5file,HighFive::File::ReadWrite | HighFive::File::Create);
//   std::vector<size_t> space_dims = {snp_idx.size(),num_elem};
//   const bool SNPfirst =	get_list_scalar<bool>(options,"SNPfirst").value_or(true);

//   if(!SNPfirst){
//     std::reverse(space_dims.begin(),space_dims.end());
//   }

//   DataSpace space(space_dims);
//   const int buffer_size= get_list_scalar<int>(options,"buffer_size").value_or(10000);
//   Filter filter	= create_filter(space_dims,options);
//   auto dset = file.createDataSet(datapath,space,AtomicType<double>(),filter);


//   std::cout<<"Starting to map file"<<std::endl;
//   boost::iostreams::mapped_file_source mapfile(dosagefile.c_str());
//   boost::iostreams::stream<boost::iostreams::mapped_file_source> textstream(mapfile);
//   boost::iostreams::filtering_istream fs;
//   fs.push(boost::iostreams::gzip_decompressor{});
//   fs.push(textstream);
//   const size_t tbf=buffer_size;
//   mach_file mf(fs,names,snp_idx,mp,tbf,dset,SNPfirst);
//   mf.process_file(true);
// }






// // void read_line_chunk(){
//   //   const size_t line_beg=name_sizes[line_no]+6;
//   //   const size_t line_remaining=current_line_size-line_pos;
//   //   if(line_remaining==0){
//   //     Rcpp::stop("Line is empty!");
//   //   }
//   //   if(line_remaining<(buffer_size*6)){
//   //     fs.read(region_buffer.data(),line_remaining);
//   //     region_buffer.resize(line_remaining);
//   //   }else{
//   //     fs.read(region_buffer.data(),buffer_size*6);
//   //     region_buffer.resize(buffer_size*6);
//   //   }
//   //   line_pos+=region_buffer.size();
//   // }


// //
// //
// template<typename TB, typename TE>
// int read_geno_line(boost::iostreams::filtering_istream &fs,const Rcpp::IntegerVector &index, const int p,TB begini, TE endi){

//   std::array<char, 10> ttbuf;
//   fs.getline(ttbuf.data(),10,'\t');
//   int mname=atoi(ttbuf.data());
//   fs.getline(ttbuf.data(),10,'\t');
//   std::array<char, 6> tbuf;
//   const size_t isize=index.size();
//   if(endi-begini!=isize){
//     Rcpp::Rcerr<<"(end-begin):"<<endi-begini<<" p: "<<p<<std::endl;
//     Rcpp::stop("wrong size for input range!");
//   }
//   int i=0;
//   int index_idx = 0;
//   int retveci = 0;
//   Progress prog_bar(isize, true);
//   auto tb=begini;
//   for(int i=0;i<p;i++){
//     prog_bar.increment();
//     fs.read(tbuf.data(),6);
//     if(index_idx<isize){
//       if(i==index[index_idx]){
// 	index_idx++;
// 	sscanf(tbuf.data(),"%5lf\t",&tb);
// 	tb++;
//       }
//     }
//   }
//   return(mname);
// }
// 
// 
// 
// Rcpp::NumericVector write_genotype_h5(const std::string dosagefile,Rcpp::IntegerVector index,const int p){
// 
//
// 
//   read_geno_line(fs,index,p,retvec.begin(),retvec.end());
// 
// 
// 
//   return(retvec);
// }
// 
// 
// 
// 
// 
// 
// 
// 

// namespace ranges
// {
//     inline namespace v3
//     {
//         /// \addtogroup group-core
//         /// @{
//         template<typename Val>
//         struct filtering_istream_range
//           : view_facade<filtering_istream_range<Val>, unknown>
//         {
//         private:
//             friend range_access;
//             std::istream *sin_;
//             movesemiregular_t<Val> obj_;
//             struct cursor
//             {
//             private:
//                 filtering_istream_range *rng_;
//             public:
//                 cursor() = default;
//                 explicit cursor(filtering_istream_range &rng)
//                   : rng_(&rng)
//                 {}
//                 void next()
//                 {
//                     rng_->next();
//                 }
//                 Val &read() const noexcept
//                 {
//                     return rng_->cached();
//                 }
//                 bool equal(default_sentinel) const
//                 {
//                     return !*rng_->sin_;
//                 }
//             };
//             void next()
//             {
//                 *sin_ >> cached();
//             }
//             cursor begin_cursor()
//             {
//                 return cursor{*this};
//             }
//         public:
//             filtering_istream_range() = default;
//             explicit filtering_istream_range(std::istream &sin)
//               : sin_(&sin), obj_{}
//             {
//                 next(); // prime the pump
//             }
//             Val & cached() noexcept
//             {
//                 return obj_;
//             }
//         };

//     #if !RANGES_CXX_VARIABLE_TEMPLATES
//         template<typename Val>
//         filtering_istream_range<Val> istream(std::istream & sin)
//         {
//             CONCEPT_ASSERT_MSG(DefaultConstructible<Val>(),
//                "Only DefaultConstructible types are extractable from streams.");
//             return filtering_istream_range<Val>{sin};
//         }
//     #else
//         template<typename Val, CONCEPT_REQUIRES_(DefaultConstructible<Val>())>
//         struct filtering_istream_fn
//         {
//             filtering_istream_range<Val> operator()(std::istream & sin) const
//             {
//                 return filtering_istream_range<Val>{sin};
//             }
//         };

//     #if RANGES_CXX_INLINE_VARIABLES < RANGES_CXX_INLINE_VARIABLES_17
//         inline namespace
//         {
//             template<typename Val>
//             constexpr auto& istream = static_const<istream_fn<Val>>::value;
//         }
//     #else  // RANGES_CXX_INLINE_VARIABLES >= RANGES_CXX_INLINE_VARIABLES_17
//         template<typename Val>
//         inline constexpr filtering_istream_fn<Val> istream{};
//     #endif  // RANGES_CXX_INLINE_VARIABLES

//     #endif  // RANGES_CXX_VARIABLE_TEMPLATES
//         /// @}
//     }
// }



// template<typename ITB, typename ITE> std::vector<double> parse_genoline(ITB beg, ITE end,const std::vector<size_t> &indices){

//   using namespace boost::spirit::qi;
//   const size_t ind_size= indices.size();
//   std::vector<double> temp_vec;
//   std::vector<double> ret;
//   int name;
//   temp_vec.reserve(ind_size);
//   ret.reserve(ind_size);

//   auto check_result =  phrase_parse(beg, end,
// 				    int_ >>lit("DOSE") >>*double_,
// 				    space,name, temp_vec);
//   if(!check_result){
//     Rcpp::stop("Parsing failed!");
//   }
//   const size_t temp_vec_size =temp_vec.size();

//   for(auto &iti: indices){
//     ret.push_back(temp_vec[iti]);
//   }
//   return(ret);
// }





//Rcpp::NumericVector parse_line(std::string inp,Rcpp::IntegerVector index){
// return(Rcpp::wrap(parse_genoline(inp.begin(),inp.end(),Rcpp::as<std::vector<size_t> >(index))));
//}




// size_t read_genotype_gz(boost::iostreams::filtering_istream &fs, const size_t Nsnps, const size_t Nind,const size_t chunksize){
//   using namespace Rcpp;
//   using namespace boost::spirit::qi;
//   //  std::vector<double> genotypes;
//   std::vector<int> chroms;
//   std::vector<unsigned int> poss;
//   std::vector<float> genotypes;
//   chroms.reserve(chunksize);
//   poss.reserve(chunksize);
//   genotypes.reserve(chunksize*Nind);
//   refs.clear();
//   refs.reserve(chunksize);
//   alts.clear();
//   alts.reserve(chunksize);
//   size_t ct=0;
//   std::string line;
//   while(getline(fs,line)){
//     std::string::const_iterator sbeg = line.begin();
//     std::string::const_iterator send = line.end();
//     phrase_parse(sbeg,send,int_>>'_'>>uint_>>'_'>>as_string[+char_("ACTGN")]>>"_">>as_string[+char_("ACTGN")]>>"_">>"b37">>*float_,space,chroms,poss,refs,alts,genotypes);
//     if(refs.size()!=alts.size()){
//       Rcpp::Rcerr<<"refs and alts different sizes! at line "<<ct<<" ("<<refs.size()<<" "<<alts.size()<<")"<<std::endl;
//       Rcpp::stop("error in read_genotype_gz");
//     }
//     ct++;
//     if(ct==chunksize){
//       break;
//     }
//   }
//   mchroms=arma::conv_to<arma::uvec>::from(chroms);
//   mposs = arma::conv_to<arma::uvec>::from(poss);
//   mgenotypes = arma::fmat(&genotypes[0],Nind,ct);
//   arma::uvec sizes={mchroms.n_elem,mposs.n_elem,mgenotypes.n_cols,(arma::uword)refs.size(),(arma::uword)alts.size()};
//   if(any(sizes!=mchroms.n_elem)){
//     sizes.print();
//     Rcpp::Rcerr<<"not all sizes are equal!:"<<std::endl;
//     Rcpp::stop("error in read_genotype_gz");
//   }
//   return(ct);
// }


//   std::string title;
//   getline(fs,title);
//   size_t sr=0;
//   size_t scum=0;
//   std::cout<<"Starting to read genotype data"<<std::endl;
//   arma::uvec chroms;
//   arma::uvec poss;
//   arma::uvec retdoFlip;
//   arma::fmat genodat;
//   std::vector<std::string> refs;
//   std::vector<std::string> alts;
//   size_t count=0;
//   while(scum<Nsnps){
//     sr = read_genotype_gz(fs, Nsnps, Nind,chunksize,chroms,poss,refs,alts,genodat);
//     scum=sr+scum;
//     Rcout<<"Line "<<scum<<" of "<<Nsnps<<std::endl;
//     size_t retn =genodat.n_cols;
//     retdoFlip=isFlip(refs,alts);
//     if(doFlip){
//       std::cout<<"Flipping alleles"<<std::endl;
//       makeFlip(retdoFlip,genodat);
//     }
//   //  std::cout<<"Writing genotype matrix"<<std::endl;
//     write_mat_h5(h5file, "SNPdata", "genotype", Nsnps,Nind,genodat,deflate_level);
// //    std::cout<<"Writing retchrom"<<std::endl;
//     write_int_h5(h5file,"SNPinfo","chrom",Nsnps,chroms,deflate_level);
// //    std::cout<<"Writing retpos "<<std::endl;
//     write_uint_h5(h5file,"SNPinfo","pos",Nsnps,poss,deflate_level);
// //    std::cout<<"Writing doFlip"<<std::endl;
//     write_int_h5(h5file,"SNPinfo","doFlip",Nsnps,retdoFlip,deflate_level);
//     if(doFlip){
//       std::cout<<"Writing doFlip"<<std::endl;
//     }
//     count++;
//   }
//   return(count);
// }
