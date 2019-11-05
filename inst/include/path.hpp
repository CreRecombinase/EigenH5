#pragma once
#include <Rcpp.h>


#if __has_include(<filesystem>)

#include <filesystem>

using Path = std::filesystem::path;

inline std::string operator+(const std::string a, const Path &b){
  return(a+b.string());
}

inline std::string operator+(const Path &a, const std::string  &b){
  return(a.string()+b);
}


#else
class Path{
public:
  std::string nodes;
  Path(const Rcpp::String &name):nodes(name.get_cstring()){
  }
  Path(const std::string &name):nodes(name){
  }
  std::string parent_path() {
    if(nodes.size()==0){
      Rcpp::stop("path is empty!");
    }
    auto nodes_rend = nodes.crend();
    auto nodes_rbegin = nodes.crbegin();

    if (nodes.size() > 1 && nodes.back() == '/') {
      nodes_rbegin++;
    }
    nodes_rbegin=	std::find(nodes_rbegin,nodes_rend,'/');
    if (nodes_rbegin == nodes_rend) {
      Rcpp::stop("can't find parent path in:" + nodes);
    }
    std::string ret(nodes_rbegin,nodes_rend);
    std::reverse(ret.begin(),ret.end());
    return (ret);
  }
  const char *c_str() const {
    return(nodes.c_str());
  }
  bool has_parent_path() const{
    return nodes.find('/') != std::string::npos;
  }
  auto find(const char x){
    return nodes.find(x);
  }
  bool is_absolute() const{
    return(nodes.front()!='/');
  }

  std::string filename() {
    if(nodes.size()==0){
      Rcpp::stop("path is empty!");
    }
    auto nodes_rend = nodes.crend();
    auto nodes_rbegin = nodes.crbegin();
    auto nodes_rrbegin = nodes_rbegin;

    if (nodes.size() > 1 && nodes.back() == '/') {
      nodes_rbegin++;
    }
    nodes_rbegin=	std::find(nodes_rbegin,nodes_rend,'/');
    if (nodes_rbegin == nodes_rend) {
      Rcpp::stop("can't find parent path in:" + nodes);
    }

    std::string ret(nodes_rrbegin,nodes_rbegin);
    std::reverse(ret.begin(),ret.end());

    return (ret);
  }
  std::string string() const{
    return nodes;
  }
  operator const std::string&() const { return(nodes); }
};

inline std::string operator/(const std::string a, const Path &b){
  return(a+b.nodes);
}

inline std::ostream &operator<<(std::ostream &os, const Path &dt) {
  os << dt.nodes;
  return os;
}

#endif

inline Path expand (Path in) {
  if (in.string().size () < 1) return in;
    const char * home = getenv ("HOME");
    if (home == NULL) {
      Rcpp::Rcerr << "error: HOME variable not set,setting to ''" << std::endl;
      home="";
      // throw std::invalid_argument ("error: HOME environment variable not set.");
    }
    std::string s = in.c_str ();
    if (s[0] == '~') {
      s = std::string(home) + s.substr (1, s.size () - 1);
      return Path (s);
    } else {
      return in;
    }
}



inline Path root_path(const std::string &input) {
  Path pt=expand(Path(input));

  return pt.is_absolute() ? pt : "/" / pt;
}

inline Path root_path(const Rcpp::StringVector &input) {
  if(input.size()>1){
    Rcpp::Rcerr<<"input.size() >1, using first element"<<std::endl;
  }

  if(input.size()==0){
    Rcpp::Rcerr<<"input is empty"<<std::endl;
  }
  std::string ps = Rcpp::as<std::string>(input(0));
  return root_path(ps);
}
