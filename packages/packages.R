if(!("yaml" %in% installed.packages())){
  install.packages("yaml")
}

package_list <- yaml::read_yaml("config/packages.yml")

new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)){
  install.packages(new_packages)
}
