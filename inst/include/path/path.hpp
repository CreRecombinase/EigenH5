#pragma once
#include <algorithm>
#include <boost/regex.hpp>
#include <iterator>
#include <vector>

class PathNode{
  const bool isDirectory;
  const bool isRoot;

public:
  PathNode();
  PathNode(const std::string &node_name,const bool isDir=false);
  const std::string name;
  std::string to_name()const ;
  const char* c_str() const;
  bool is_root()const;
  const std::string& string() const;
  bool is_directory() const;
private:
  static bool is_it_root(const std::string name);
};

bool operator==(const PathNode &a, const PathNode &b);
bool operator!=(const PathNode &a, const PathNode &b);

class Path {
  using	PathVec = std::vector<PathNode>;
  using PathVecIt = PathVec::const_iterator;
  std::vector<PathNode> nodes;
  bool isAbsolute;
  bool PathToDirectory;
  std::string string_rep;
public:
  Path();
  Path(const std::string &path);
  Path(const PathVecIt beg_it, const PathVecIt end_it);
  const std::string &string() const;
  const char* c_str()const;
  bool has_parent_path() const;
  Path parent_path()const ;
  PathNode filename() const;
  PathVecIt begin() const;
  PathVecIt end() const;
  bool is_directory() const;
  bool is_absolute() const;
private:
  void update_string_rep();
  Path(std::vector<PathNode> nodes_, bool isAbsolute_,
       bool PathToDirectory_);
};
