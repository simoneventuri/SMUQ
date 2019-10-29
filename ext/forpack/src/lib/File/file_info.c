#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#if defined(_WIN32) && defined(__INTEL_COMPILER)
#  include "dirent_windows.h"
#else
#  include <dirent.h>
#endif

void file_info(const char*filename,int*mode,int*exist,int*time){
  int k;
  struct stat buf;
  k=stat(filename,&buf);
  if(k != 0) {
    *mode=0;
    *exist=0;
    *time=0;
  }else{
    *mode=buf.st_mode;
    if(*mode == 0) *exist=0; else *exist=1;
    *time=buf.st_mtime;
  }
}