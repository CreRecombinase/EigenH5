#include "EigenH5.h"





// std::string get_datapath(const Rcpp::List &list){
//   auto dp = get_list_scalar<std::string>(list,"datapath");
//   if(dp){
//     return(*dp);
//   }else{
//     auto gp = get_list_any<std::string>(list,{"group","groupname","Group","Groupname","Groups","groups"}).value_or("/");
//     auto dsp =get_list_any<std::string>(list,{"dataset","dataname","Dataset","Data","Datanames","datanames"}).value_or("");
//     return(stdx::filesystem::path(gp)/stdx::filesystem::path(dsp));
//   }
// }
