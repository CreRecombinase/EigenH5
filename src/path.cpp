#include "EigenH5.h"


PathNode::PathNode() : isDirectory(true), isRoot(true) {}

PathNode::PathNode(const std::string &node_name,const bool isDir)
    : isDirectory(isDir), isRoot(is_it_root(node_name)),
      name(node_name) {
    if (std::any_of(name.begin(), name.end(),
                    [](const char i) { return i == '/'; })) {
      Rcpp::Rcerr << "In PathNode(" + name + ")" << std::endl;
      Rcpp::stop("PathNode cannot contain delimiters (/)");
    }
  }

std::string PathNode::to_name() const{
    if(isRoot){
      return("/");
    }
    if (isDirectory) {
      return (name + "/");
    }
    return(name);
}
const char* PathNode::c_str() const{
    return(name.c_str());
  }
bool PathNode::is_root()const {
    return(isRoot);
  }
const std::string& PathNode::string() const{
    return name;
  }
bool PathNode::is_directory() const{ return (isDirectory); }

bool PathNode::is_it_root(const std::string name){
  if (name.size() == 0) {
    return true;
  }
  return (name.size() == 1 && name.back() == '/');
}

bool operator==(const PathNode &a, const PathNode &b) {
  return ((a.name == b.name) && (a.is_directory() == b.is_directory()) &&
          (a.is_root() == b.is_root()));
}
bool operator!=(const PathNode &a, const PathNode &b) { return (!(a == b)); }


Path::Path() : nodes{PathNode()}, isAbsolute(true), PathToDirectory(true) {}
Path::Path(const std::string &path){
  if (path.size() == 0) {
    nodes.push_back(PathNode());
    isAbsolute = true;
    PathToDirectory = true;
  } else {
    isAbsolute = (path.front() == '/');
    PathToDirectory = (path.back() == '/');

    std::regex dir_split("/+");

    auto match_it = std::sregex_token_iterator(path.begin(), path.end(), dir_split,-1);
    auto match_e = std::sregex_token_iterator();
    std::string tmatch;
    do {
      tmatch = match_it->str();
      match_it++;
      nodes.emplace_back(tmatch, match_it!=match_e || PathToDirectory);
    } while (match_it!=match_e);
    update_string_rep();
  }
}
Path::Path(const PathVecIt beg_it, const PathVecIt end_it):nodes(beg_it,end_it){
    if (beg_it == end_it) {
      Rcpp::stop("Cannot create empty Path");
    }
    isAbsolute = beg_it->is_root();
    PathToDirectory = nodes.back().is_directory();
    this->update_string_rep();
  }

const std::string& Path::string() const { return (string_rep); }

const char* Path::c_str()const{
    return(string_rep.c_str());
  }
bool Path::has_parent_path() const {
    return nodes.size()>1;
  }
Path Path::parent_path()const {
    if (!has_parent_path()) {
      Rcpp::stop("Path: " + string_rep + " does not have parent path!");
    }
    std::vector<PathNode> retnodes = nodes;
    retnodes.pop_back();
    return (Path(retnodes, isAbsolute, true));
  }
PathNode Path::filename() const{
    return(nodes.back());
  }
Path::PathVecIt Path::begin() const{
    return(nodes.begin());
  }
Path::PathVecIt  Path::end() const{
    return(nodes.end());
  }
bool Path::is_directory() const{
    return PathToDirectory;
  }
bool Path::is_absolute() const{
    return isAbsolute;
  }
void Path::update_string_rep(){
  string_rep.clear();
  for (auto &tn : nodes) {
    string_rep += tn.to_name();
  }
}
Path::Path(std::vector<PathNode> nodes_, bool isAbsolute_,
	   bool PathToDirectory_)
  : nodes(nodes_), isAbsolute(isAbsolute_),
    PathToDirectory(PathToDirectory_) {
  this->update_string_rep();
}
