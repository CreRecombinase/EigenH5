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

// template<typename T>
// class data_slice{

// public:
//   data_slice(const size_t  capacity, std::initializer_list<size_t>  off,std::initializer_list<size_t> chunk):
//     offset(off),
//     chunksize(chunk){
//     dat.reserve(capacity);
//   }
//   void reset_slice(){
//     dat.clear();
//     chunksize = {0,0};
//     offset = {0,0};
//   std::vector<T> dat;
//   std::vector<size_t> offset;
//   std::vector<size_t> chunksize;
// };


template<typename T>
class buffered_writer{
  const size_t vec_capacity;
  const bool SNPfirst;
  std::vector<T> data_buff;
  HighFive::File file;
  HighFive::DataSet  ds;
  size_t offset_snp;
  size_t offset_sample;
  bool is_empty;
  const size_t max_offset_snp;
  const size_t max_offset_sample;
public:
  buffered_writer(const std::string filename, const std::string datapath,const size_t max_buffer_size,const bool SNPfirst_, const size_t num_snp, const size_t num_n):
    vec_capacity(max_buffer_size),
    file(filename,HighFive::File::ReadWrite | HighFive::File::Create),
    ds(file.getDataSet(datapath)),SNPfirst(SNPfirst_),is_empty(true),max_offset_snp(num_snp),
    max_offset_sample(num_n){
    data_buff.reserve(vec_capacity);
  }
  void push_data(T datum,const size_t snp_offset,size_t sample_offset){
    if((data_buff.size()>=vec_capacity) || (sample_offset != offset_sample) ) {
      write_data();
    }
    data_buff.push_back(datum);
    if(is_empty){
      offset_snp=snp_offset;
      offset_sample=sample_offset;
      is_empty=false;
    }
  }
  void write_data(){
    if(!is_empty){
      if(offset_snp+data_buff.size()> max_offset_snp){
	Rcpp::Rcerr<<"Sample  offset+chunksize: "+std::to_string(offset_sample)+",1,"<<max_offset_sample<<std::endl;
	Rcpp::stop("cannot perform write, SNP offset,chunksize,max: "+std::to_string(offset_snp)+","+std::to_string(data_buff.size())+","+std::to_string(max_offset_snp));
      }
      if(offset_sample+1>max_offset_sample){
	Rcpp::stop("cannot perform write, Sample  offset+chunksize: "+std::to_string(offset_sample)+",1");
      }
      std::vector<size_t> off= {offset_snp,offset_sample};
      std::vector<size_t> csize= {data_buff.size(),1};
      if(!SNPfirst){
	std::reverse(off.begin(),off.end());
	std::reverse(csize.begin(),csize.end());
      }
      ds.select(off,csize,{}).write(data_buff);
      data_buff.clear();
      is_empty=true;
    }
  }
  ~buffered_writer() {
    write_data();
  }
};


template<typename string_type>
inline bool str_to_value(const string_type& src, float& dest)
{
  namespace qi = boost::spirit::qi;
  
  return qi::parse(std::cbegin(src), std::cend(src), qi::float_, dest);
}



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


class Region_buffer{
public:
  std::string buffer;
  std::pair<int,int> pos;
  const size_t max_size;
  Region_buffer(const size_t ms):max_size(ms),pos{0,0}{
    buffer.reserve(max_size);
  };
  const size_t size()const{
    return(buffer.size());
  }
  void empty(){
    pos={0,pos.second};
    buffer.clear();
  }
  void reset(){
    pos={0,0};
    buffer.clear();
  }
};


template<typename T>
class Mach_file{
  boost::iostreams::filtering_istream &fs;
  mutable std::unordered_map<std::string,size_t> sample_names;
  const size_t num_rows;
  const std::vector<int> &snp_indices;
  const size_t snp_ind_size;
  const size_t p;

  buffered_writer<T> & bw;
  Progress prog_bar;
  //  Region_buffer region_buffer;
  // std::string region_buffer;
  //  size_t line_pos; //line cursor (should always be inside the buffer)
  size_t snp_idx; //Which snp index am I on?
  size_t line_no; // Which sample am I on?
  //  std::pair<int,int> buffer_pos; //Which byte in the buffer am I on (start and one past end)?

public:
  Mach_file(boost::iostreams::filtering_istream	&fs_,
	    const std::vector<std::string> &sample_names_,
	    const std::vector<int> &snp_indices_, const size_t p_,
	    buffered_writer<T> & bw_,const bool progress=true):
    fs(fs_),
    num_rows(sample_names_.size()),
    snp_indices(snp_indices_),
    snp_ind_size(snp_indices.size() ),
    p(p_),
    bw(bw_),
    prog_bar(num_rows,progress)
  {
    for(size_t i=0; i<num_rows; i++){
      sample_names.insert({sample_names_[i],i});
    }
    line_no=0;

  }
private:
  size_t snp_line_pos(const size_t idx) const{
    const size_t cur_snp=snp_indices[idx];
    return(cur_snp*6);
  }



  std::optional<size_t> get_current_sample(const std::string &t_sample_id)const{
    auto ret = sample_names.find(t_sample_id);
    //std::find(sample_names.begin(),sample_names.end(),t_sample_id);
    if(ret == sample_names.end()){
      return(std::nullopt);
    }else{
      size_t offset = ret->second;
      sample_names.erase(ret);
      return(offset);
    }
  }

  void parse_buffer(const size_t sample_offset, const Region_buffer &region_buffer){
    //stuff the buffered SNPs into the data vector until:
    //  the buffer is empty
    std::string_view tbuff(region_buffer.buffer);
    //first snp in the buffer is always cur_snp,
    //last snp in the buffer is in snp_indices
    T tres;

    for(int i=0; i<snp_indices.size();i++)
      {
	auto tb = tbuff.substr(snp_line_pos(i),6);
	str_to_value(tb,tres);
	bw.push_data(tres,i,sample_offset);
      }

  }
  std::optional<size_t> read_good_line(Region_buffer &region_buffer){
    if(!fs.eof()){
      std::string sample_id;
      std::getline(fs,sample_id,'\t');
      if(fs.eof()){
	return(std::nullopt);
      }
      // read and check that we're at "DOSE"
      std::string tst;
      std::getline(fs,tst,'\t');
      if(tst != "DOSE"){
	Rcpp::stop("Not at the beginning of line_no:"+std::to_string(line_no)+":\n"+tst+"\nExpecting:\nDOSE");
      }
      auto ret = get_current_sample(sample_id);
      while(!ret.has_value()){
	fs.ignore(6*p);
	if(fs.eof()){
	  return(ret);
	}
	std::getline(fs,sample_id,'\t');
	std::getline(fs,tst,'\t');
	if(tst != "DOSE"){
	  Rcpp::stop("Not at the beginning of line_no:"+std::to_string(line_no)+":\n"+tst+"\nExpecting:\nDOSE");
	}
	ret = get_current_sample(sample_id);
      }
      std::getline(fs,region_buffer.buffer,'\n');
      region_buffer.pos={0,6*p};
      return(ret);
    }else{
      return(std::nullopt);
    }
  }


    //now read all the SNPs;



public:


  void process_file(const size_t max_buffer_size,const bool progress=true){
    //allocate buffer
    Region_buffer region_buffer(max_buffer_size);
    
    std::string sample_id;
    while(std::optional<size_t> samp_offset_o = read_good_line(region_buffer)){
      parse_buffer(samp_offset_o.value(),region_buffer);
      region_buffer.empty();
      if (Progress::check_abort() ){
	Rcpp::stop("Process Interrupted!");
      }
      prog_bar.increment();
    }
  }
};

//[[Rcpp::export]]
void mach2h5(const std::string dosagefile, const std::string h5file, const std::string datapath,std::vector<int> snp_idx, std::vector<std::string> names, const int p,Rcpp::List options){

  const size_t num_elem=names.size();
  const size_t num_snps=snp_idx.size();
  size_t mp=p;
  // const bool SNPfirst =	get_list_scalar<bool>(options,"SNPfirst").value_or(true);
  const size_t buffer_size= static_cast<size_t>(get_list_scalar<int>(options,"buffer_size").value_or(p*6));
  const bool prog= get_list_scalar<bool>(options,"progress").value_or(false);
  const bool store_float= get_list_scalar<bool>(options,"float").value_or(false);
  

  const int buffer_vec= static_cast<size_t>(get_list_scalar<int>(options,"buffer_vec").value_or(buffer_size/6));


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
  if(store_float){
    buffered_writer<float> bw(h5file,datapath,buffer_vec,SNPfirst,num_snps,num_elem);
    Mach_file mf(fs,names,snp_idx,mp,bw,prog);
    mf.process_file(buffer_size,prog);
  }else{
    buffered_writer<double> bw(h5file,datapath,buffer_vec,SNPfirst,num_snps,num_elem);
    Mach_file mf(fs,names,snp_idx,mp,bw,prog);
    mf.process_file(buffer_size,prog);
  }
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
