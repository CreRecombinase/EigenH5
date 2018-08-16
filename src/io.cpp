#include <boost/iostreams/stream_buffer.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/utility/string_ref.hpp>
#include <boost/spirit/include/qi.hpp>
//#include <charconv>
//#include <gsl/span>
#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>
#include <stddef.h>

// [[Rcpp::interfaces(r, cpp)]]

template<typename T>
class data_slice{

public:
  data_slice(const std::vector<T> & tdat, std::initializer_list<size_t>  off,std::initializer_list<size_t> chunk):
    dat(tdat),
    offset(off),
    chunksize(chunk){}
  std::vector<T> dat;
  std::vector<size_t> offset;
  std::vector<size_t> chunksize;
};


template<typename T>
class buffered_writer{
  const size_t buffer_capacity;
  std::vector<data_slice<T> > data_queue;
  HighFive::File file;
  HighFive::DataSet  ds;
public:
  buffered_writer(const std::string filename, const std::string datapath, const size_t buffer_cap):
    buffer_capacity(buffer_cap),
    file(filename,HighFive::File::ReadWrite | HighFive::File::Create),
    ds(file.getDataSet(datapath)){
    data_queue.reserve(buffer_capacity);
  }
  bool push_buffer(data_slice<T> && data_el){
    while(data_queue.size()>=buffer_capacity){
      write_data();
    }
    data_queue.emplace_back(data_el);
    return(true);
  }
  void write_data(){
    if(!data_queue.empty()){
      auto b= data_queue.back();
      ds.select(b.offset,b.chunksize,{}).write(b.dat);
      data_queue.pop_back();
    }
  }
  ~buffered_writer() {
    while(!data_queue.empty()){
      write_data();
    }
  }

};

template<typename string_type>
inline bool str_to_value(const string_type& src, double& dest)
{
    namespace qi = boost::spirit::qi;

    return qi::parse(std::cbegin(src), std::cend(src), qi::double_, dest);
}

template<typename string_type>
inline bool str_to_value(const string_type& src, int& dest)
{
    namespace qi = boost::spirit::qi;

    return qi::parse(std::cbegin(src), std::cend(src), qi::int_, dest);
}

struct data_buff{
  std::string buffer;
  std::pair<int,int> pos;
  std::string_view tbuff;
};


class mach_file{
  boost::iostreams::filtering_istream &fs;
  const std::vector<std::string> &sample_names;
  const size_t num_rows;
  const std::vector<int> &snp_indices;
  const size_t snp_ind_size;
  const size_t p;
  const size_t max_buffer_size;
  const bool SNPfirst;
  buffered_writer<double> & bw;
  //  const std::vector<size_t> line_sizes;
  Progress prog_bar;
  std::string region_buffer;
  std::string current_sample;
  //  size_t line_pos; //line cursor (should always be inside the buffer)
  size_t snp_idx; //Which snp index am I on?
  size_t line_no; // Which sample am I on?
  std::pair<int,int> buffer_pos; //Which byte in the buffer am I on (start and one past end)?
  size_t sample_offset;
  std::string	sample_id;
  std::vector<double> data;
public:
  mach_file(boost::iostreams::filtering_istream	&fs_,
	    const std::vector<std::string> &sample_names_,
	    const std::vector<int> &snp_indices_, const size_t p_,
	    const size_t buffer_size_,buffered_writer<double> & bw_,const bool SNPfirst_=true,const bool progress=true):
    fs(fs_),
    sample_names(sample_names_),
    num_rows(sample_names.size()),
    snp_indices(snp_indices_),
    snp_ind_size(snp_indices.size() ),
    p(p_),
    max_buffer_size(buffer_size_),
    bw(bw_),
    SNPfirst(SNPfirst_),
    prog_bar(num_rows,progress)
  {
    line_no=0;
    region_buffer.reserve(max_buffer_size);
    data.reserve(max_buffer_size/6);
    buffer_pos={0,0};
    snp_idx=0;
    get_current_sample();
    //    size_t empty_rsize=name_sizes[0]+6;
  }
private:
  size_t snp_line_pos(const size_t idx) const{
    const size_t cur_snp=snp_indices[idx];
    //    const size_t empty_rsize=name_sizes[line_no]+6;
    return(cur_snp*6);
  }
  bool scan_until_snp(){
    const size_t cur_snp_pos = snp_line_pos(snp_idx);
    if(cur_snp_pos>=buffer_pos.second){
      fs.ignore(cur_snp_pos-buffer_pos.second);
      buffer_pos.second=cur_snp_pos;
      buffer_pos.first=cur_snp_pos;
      return(true);
    }else{
      size_t buff_ahead=cur_snp_pos-buffer_pos.first;
      region_buffer.substr(buff_ahead,buffer_pos.second-cur_snp_pos);
      return(false);
    }
  }

  void extend_buffer(){
    const size_t cur_snp_pos = snp_line_pos(snp_idx);
    size_t prel_snp=snp_idx;
    size_t prel_pos=snp_line_pos(prel_snp)+6;
    while((prel_snp+1)<snp_ind_size){
      if(((snp_line_pos(prel_snp+1)-cur_snp_pos)<(max_buffer_size))){
	prel_snp++;
	prel_pos=snp_line_pos(prel_snp)+6;
      }else{
	break;
      }
    }
    region_buffer.resize(prel_pos-cur_snp_pos);
    fs.read(region_buffer.data(),prel_pos-cur_snp_pos);
    buffer_pos.second=prel_pos;
  }

  bool get_current_sample(){
    std::getline(fs,sample_id,'\t');
    if(fs.eof()){
      return(false);
    }else{
      auto ret = std::find(sample_names.begin(),sample_names.end(),sample_id);
      if(ret == sample_names.end()){
	Rcpp::stop("sample_id: "+sample_id+" not found!");
      }else{
	//	Rcpp::Rcerr<<"sample_id: "<<sample_id<<" found on line"<<line_no<<std::endl;
	sample_offset=ret-sample_names.begin();
      }
      std::string tst;
      std::getline(fs,tst,'\t');
      // fs.read(region_buffer.data(),5);
      if(tst != "DOSE"){
	Rcpp::stop("Not at the beginning of line_no:"+std::to_string(line_no)+":\n"+tst+"\nExpecting:\nDOSE");
      }
    }
    return(true);
  }
  bool advance_line(){
    const size_t line_remaining=p*6-buffer_pos.second;
    fs.ignore(line_remaining);
    line_no++;
    //    region_buffer.resize(name_sizes[line_no]+6);
    if(get_current_sample()){
      buffer_pos={0,0};
      region_buffer.clear();
      prog_bar.increment();
      if (Progress::check_abort() ){
	Rcpp::stop("Process Interrupted!");
      }
      snp_idx=0;
      return(true);
    }else{
      return(false);
    }
  }
  bool parse_chunk(){
    //stuff the buffered SNPs into the data vector until:
    //1. the data vector is  full (return true)
    //2. the buffer is empty (return false)
    // If 1. We'll "artificially" advance the buffer to	the next SNP
    std::string_view tbuff(region_buffer);
    size_t buffer_size=buffer_pos.second-buffer_pos.first;
    size_t n_buffer_snps=buffer_size/6;
    size_t cur_snp_pos =snp_line_pos(snp_idx);
    size_t cur_snp=snp_indices[snp_idx];
    size_t buffer_snp=cur_snp;
    size_t start_snp=cur_snp;
    //first snp in the buffer is always cur_snp,
    //last snp in the buffer is in snp_indices

    double tres;

    for(int i=0; i<n_buffer_snps;i++){
      buffer_snp=start_snp+i;
      if(buffer_snp == cur_snp){
	str_to_value(tbuff,tres);
	data.push_back(tres);
	buffer_size-=6;
	buffer_pos.first+=6;
	tbuff=tbuff.substr(6,buffer_size);
	if((snp_idx==(snp_ind_size-1)) || (data.capacity()==data.size())){
	  write_buffer();
	  if(snp_idx==(snp_ind_size-1)){
	    if((line_no+1)<num_rows){
	      return(advance_line());
	    }else{
	      return(false);
	    }
	  }
	}
	snp_idx++;
	cur_snp=snp_indices[snp_idx];
      }else{
	buffer_size-=6;
	buffer_pos.first+=6;
	tbuff=tbuff.substr(6,buffer_size);
      }
    }
    region_buffer=tbuff;
    return(true);
  }
  //  size_t sample_offset(const std::string &sample_id){
  void write_buffer(){
    const size_t data_size=data.size();
    const size_t data_start=(snp_idx>data_size) ? snp_idx-(data_size)+1 : (data_size-snp_idx-1);
    //    prog_bar.increment(data_size);
    // Rcpp::Rcerr<<"Writing sample_id: "<<sample_id<<" with data_offset: "<<data_start<<" sample_offset"<<sample_offset<<std::endl;
    // Rcpp::NumericVector	tdat = Rcpp::wrap(data);
    // Rcpp::Rcerr<<"sample_data: "<<tdat<<std::endl;
    if(SNPfirst){
      bw.push_buffer(data_slice<double>(data,{data_start,sample_offset},{data_size,1}));
    }else{
      bw.push_buffer(data_slice<double>(data,{sample_offset,data_start},{1,data_size}));
    }
    data.clear();
    //    data_start+=datasize;
  }
public:
  void process_file(const bool progress=true){
    do{
      if(scan_until_snp()){
	extend_buffer();
      }
    }while(parse_chunk());

  }
};

//[[Rcpp::export]]
void mach2h5(const std::string dosagefile, const std::string h5file, const std::string datapath,std::vector<int> snp_idx, std::vector<std::string> names, const int p,Rcpp::List options){

  const size_t num_elem=names.size();
  const size_t num_snps=snp_idx.size();
  size_t mp=p;
  // const bool SNPfirst =	get_list_scalar<bool>(options,"SNPfirst").value_or(true);
  const int buffer_size= get_list_scalar<int>(options,"buffer_size").value_or(10000);
  const bool prog= get_list_scalar<bool>(options,"progress").value_or(false);


  const int buffer_vec= static_cast<size_t>(get_list_scalar<int>(options,"buffer_vec").value_or(1));


  const bool SNPfirst =   [h5file,datapath,num_snps,num_elem](){
			    using namespace HighFive;
			    File file(h5file,HighFive::File::ReadOnly);
			    auto dset =	file.getDataSet(datapath);
			    auto data_dims = dset.getDataDimensions();
			    if(data_dims[0]==num_snps){
			      if(data_dims[1]==num_elem){
				return(true);
			      }else{
				Rcpp::stop("SNPfirst but second dimension doesn't match length(names)");
			      }
			    }else{
			      if(data_dims[1]==num_snps){
				if(data_dims[0]==num_elem){
				  return(false);
				}else{
				  Rcpp::stop("!SNPfirst but first dimension doesn't match length(names)");
				}
			      }else{
				Rcpp::stop("first dimension doesn't match length(snp_idx) or length(names)");
			      }
			    }
			  }();


  boost::iostreams::mapped_file_source mapfile;
  mapfile.open(dosagefile);
  if(!mapfile.is_open()){
    Rcpp::stop("opening	file:"+dosagefile+"failed!");
  }else{
    // Rcpp::Rcerr<<"File mapped, opening stream"<<std::endl;
  }
  boost::iostreams::stream<boost::iostreams::mapped_file_source> textstream(mapfile);
  boost::iostreams::filtering_istream fs;
  fs.push(boost::iostreams::gzip_decompressor{});
  fs.push(textstream);
  const size_t tbf=buffer_size;
  buffered_writer<double> bw(h5file,datapath,buffer_vec);
  mach_file mf(fs,names,snp_idx,mp,tbf,bw,SNPfirst,prog);
  mf.process_file(prog);
}






// void read_line_chunk(){
  //   const size_t line_beg=name_sizes[line_no]+6;
  //   const size_t line_remaining=current_line_size-line_pos;
  //   if(line_remaining==0){
  //     Rcpp::stop("Line is empty!");
  //   }
  //   if(line_remaining<(buffer_size*6)){
  //     fs.read(region_buffer.data(),line_remaining);
  //     region_buffer.resize(line_remaining);
  //   }else{
  //     fs.read(region_buffer.data(),buffer_size*6);
  //     region_buffer.resize(buffer_size*6);
  //   }
  //   line_pos+=region_buffer.size();
  // }


//
//
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
