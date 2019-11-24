/** @file package.cc
 *
 *   The FORM package manager.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */

#ifdef HAVE_CONFIG_H
#ifndef CONFIG_H_INCLUDED
#define CONFIG_H_INCLUDED
#include <config.h>
#endif
#endif

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <string>

extern "C" {
#include "form3.h"
}

namespace {

/**
 * Returns a copy of the given string with removing whitespace both at the
 * beginning and the end.
 */
std::string Trim(const std::string& s) {
  std::size_t i = 0;
  std::size_t j = 0;
  std::size_t k;
  k = s.find_first_not_of(" \t");
  if (k != std::string::npos) {
    i = k;
    std::size_t k = s.find_last_not_of(" \t");
    if (k != std::string::npos) {
      j = k + 1;
    }
  }
  return s.substr(i, j - i);
}

/**
 * Checks if the file with the given name exists.
 */
bool FileExists(const std::string& filename) {
  return std::ifstream(filename.c_str()).is_open();
}

/**
 * Returns the path to the current user's home directory.
 */
std::string GetHomeDirectory() {
  // TODO: more portability
  const char* home = std::getenv("HOME");
  if (home) return home;

  home = std::getenv("HOMEPATH");
  if (home) return home;

  return "";
}

//@{

/**
 * Joins two or more path components.
 */
std::string JoinPath(const std::string& path1, const std::string& path2) {
  // Make copies.
  std::string s1 = path1;
  std::string s2 = path2;
  // Canonicalize the paths in such a way that:
  // (1) both path1 and path2 don't have the separator at the end.
  // (2) path2 doesn't have the separator at the beginning.
  if (!s1.empty()) {
    if (s1[s1.length() - 1] == SEPARATOR ||
        s1[s1.length() - 1] == ALTSEPARATOR) {
      s1.erase(s1.length() - 1);
    }
  }
  if (!s2.empty()) {
    if (s2[0] == SEPARATOR || s2[0] == ALTSEPARATOR) {
      s2.erase(0);
    }
  }
  if (!s2.empty()) {
    if (s2[s2.length() - 1] == SEPARATOR ||
        s2[s2.length() - 1] == ALTSEPARATOR) {
      s2.erase(s2.length() - 1);
    }
  }
  // Join the two paths.
  return s1 + SEPARATOR + s2;
}

std::string JoinPath(const std::string& path1, const std::string& path2,
                     const std::string& path3) {
  return JoinPath(path1, JoinPath(path2, path3));
}

std::string JoinPath(const std::string& path1, const std::string& path2,
                     const std::string& path3, const std::string& path4) {
  return JoinPath(path1, JoinPath(path2, path3, path4));
}

std::string JoinPath(const std::string& path1, const std::string& path2,
                     const std::string& path3, const std::string& path4,
                     const std::string& path5) {
  return JoinPath(path1, JoinPath(path2, path3, path4, path5));
}

std::string JoinPath(const std::string& path1, const std::string& path2,
                     const std::string& path3, const std::string& path4,
                     const std::string& path5, const std::string& path6) {
  return JoinPath(path1, JoinPath(path2, path3, path4, path5, path6));
}

//@}

/**
 * Escapes the given path name.
 */
std::string EscapePath(const std::string& path) {
  std::string s = path;
  std::replace(s.begin(), s.end(), SEPARATOR, '_');
  std::replace(s.begin(), s.end(), ALTSEPARATOR, '_');
  std::replace(s.begin(), s.end(), ':', '_');
  std::replace(s.begin(), s.end(), '@', '_');
  std::replace(s.begin(), s.end(), ' ', '_');
  return s;
}

/**
 * Checks if the given file starts with "404" (indicating "File Not Found" on
 * GitHub).
 */
bool Is404(const std::string& filename) {
  char buf[3];
  std::ifstream file;
  file.open(filename.c_str(), std::ios::in | std::ios::binary);
  file.read(buf, 3);
  return file && buf[0] == '4' && buf[1] == '0' && buf[2] == '4';
}

/**
 * Deploys a package.
 */
int DeployPackage(const std::string& path, const std::string& url) {
  // NOTE: we assume decent UNIX systems.
#ifdef UNIX
  std::string cmd;
  int err;

  // Create a temporary working directory.

  std::string pid = (const char*)GetPreVar((UBYTE*)"PID_", WITHERROR);

  std::string tmp_dir = "xformpkg" + pid;

  cmd = "mkdir -p " + tmp_dir;
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  // Download the package into the temporary directory.

  std::string tmp_name = JoinPath(tmp_dir, EscapePath(url));

  cmd = "curl -L -o " + tmp_name + " " + url + " 2>&1";
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  if (Is404(tmp_name)) {
    return 404;
  }

  // Uncompress the downloaded package.

  cmd = "cd " + tmp_dir + " && tar xfz *.tar.gz";
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  // Relocate the package to the destination path.

  cmd = "mkdir -p $(dirname " + path + ")";
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  cmd = "mv $(ls -d " + tmp_dir + "/*/) " + path;
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  // Create the ".complete" file.

  cmd = "touch " + JoinPath(path, ".complete");
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  // Delete the working directory.

  cmd = "rm -rf " + tmp_dir;
  err = DoSystem((UBYTE*)cmd.c_str());
  if (err) return err;

  // Congratulations! The package installation succeeded.

  return 0;
#else
  MesPrint("Package installer not implemented on this system");
  return -1;
#endif
}

/**
 * Returns the package installation path.
 */
std::string GetPackagePath(const std::string& repo, const std::string& group,
                           const std::string& name,
                           const std::string& version) {
  return JoinPath(GetHomeDirectory(), ".form", repo, group, name, version);
}

/**
 * Returns the package short name.
 */
std::string GetPackageDisplayName(const std::string& repo,
                                  const std::string& group,
                                  const std::string& name,
                                  const std::string& version) {
  if (repo == "github.com") {
    return group + "/" + name + "@" + version;
  } else {
    return repo + "/" + group + "/" + name + "@" + version;
  }
}

/**
 * Returns the package URL.
 */
std::string GetPackageURL(const std::string& repo, const std::string& group,
                          const std::string& name, const std::string& version) {
  if (repo == "github.com") {
    return "https://github.com/" + group + "/" + name + "/archive/" + version +
           ".tar.gz";
  } else if (repo == "gitlab.com") {
    return "https://gitlab.com/" + group + "/" + name + "/-/archive/" +
           version + "/" + name + "-" + version + ".tar.gz";
  } else if (repo == "bitbucket.org") {
    return "https://bitbucket.org/" + group + "/" + name + "/get/" + version +
           ".tar.gz";
  } else {
    return "https://" + repo + "/" + group + "/" + name + "/" + version +
           ".tar.gz";
  }
}

/**
 * Ensures that the specified package is deployed.
 */
int EnsurePackage(const std::string& repo, const std::string& group,
                  const std::string& name, const std::string& version,
                  bool silent = true) {
  std::string package_display_name =
      GetPackageDisplayName(repo, group, name, version);
  std::string package_path = GetPackagePath(repo, group, name, version);

  std::string complete_file = JoinPath(package_path, ".complete");

  if (!FileExists(complete_file)) {
    MesPrint("@Download package: %s", package_display_name.c_str());

    std::string package_url = GetPackageURL(repo, group, name, version);
    int err = DeployPackage(package_path, package_url);
    if (err == 404) {
      MesPrint("@404: File Not Found");
      return -1;
    }
    if (err) return err;

    MesPrint("@Installed: %s", package_path.c_str());
  } else if (!silent) {
    MesPrint("@Already installed: %s", package_path.c_str());
  }
  return 0;
}

/**
 * Splits a package name into its components.
 */
int SplitPackageNameComponents(const std::string& str, std::string* repo,
                               std::string* group, std::string* name,
                               std::string* version) {
  std::string s = Trim(str);
  bool rest_ignored = false;

  // Check if the string still has spaces.
  if (!s.empty()) {
    std::size_t i = s.find_first_of(" \t");
    if (i != std::string::npos) {
      rest_ignored = true;
      s.erase(i);
    }
  }

  // Find separators in the string.
  std::size_t i = s.find_first_of("/");
  std::size_t j = s.find_first_of("/", i + 1);
  std::size_t k = s.find_last_of("@");
  std::size_t l, m;
  if (i == std::string::npos) {
    goto illegal_format;
  }
  if (k == std::string::npos) {
    goto illegal_format;
  }

  // More checks.
  l = s.find_first_of("@");
  m = s.find_first_of("/", k + 1);
  if (l != k) {
    goto illegal_format;
  }
  if (m != std::string::npos) {
    goto illegal_format;
  }

  // Split it.
  if (j != std::string::npos && j < k) {
    if (!(0 < i && i + 1 < j && j + 1 < k && k + 1 < s.size())) {
      goto illegal_format;
    }
    *repo = s.substr(0, i);
    *group = s.substr(i + 1, j - (i + 1));
    *name = s.substr(j + 1, k - (j + 1));
    *version = s.substr(k + 1);
  } else {
    if (!(0 < i && i + 1 < k && k + 1 < s.size())) {
      goto illegal_format;
    }
    *repo = "github.com";
    *group = s.substr(0, i);
    *name = s.substr(i + 1, k - (i + 1));
    *version = s.substr(k + 1);
  }

  if (rest_ignored) {
    MesPrint("&Text after whitespace in a package name is ignored");
  }

  return 0;

illegal_format:
  MesPrint("&Illegal package name: %s", s.c_str());
  return -1;
}

}  // unnamed namespace

/**
 * Installs the specified package.
 *
 * Syntax:
 *   form -install [<site>/]<group>/<name>@<tag>
 */
extern "C" int InstallPackage(UBYTE* s) {
  int err;

  std::string repo;
  std::string group;
  std::string name;
  std::string version;

  err = SplitPackageNameComponents(Trim(std::string((const char*)s)), &repo,
                                   &group, &name, &version);
  if (err) return err;

  err = EnsurePackage(repo, group, name, version, false);
  if (err) return err;

  return 0;
}

/**
 * Loads the specified package. If the package is unavailable, it will be
 * automatically downloaded and installed.
 *
 * Syntax:
 *   #UsePackage [-+] [<site>/]<group>/<name>@<tag> [: <include-arguments>]
 */
extern "C" int DoPreUsePackage(UBYTE* s) {
  if (AP.PreSwitchModes[AP.PreSwitchLevel] != EXECUTINGPRESWITCH) return (0);
  if (AP.PreIfStack[AP.PreIfLevel] != EXECUTINGIF) return (0);

  int err;

  int sign = 0;  // "+" (1) or "-" (-1) or none (0).

  std::string line = Trim(std::string((const char*)s));
  std::string incl_args;

  // Check the optional sign.
  if (!line.empty() && (line[0] == '+' || line[0] == '-')) {
    sign = line[0] == '+' ? 1 : -1;
    line.erase(0, 1);
    line = Trim(line);
  }

  // Check the optional include arguments.
  {
    std::size_t i = line.find_first_of(":");
    if (i != std::string::npos) {
      incl_args = Trim(line.substr(i + 1));
      line = Trim(line.substr(0, i));
    }
  }

  // First, we need to "Ensure Package".

  std::string repo;
  std::string group;
  std::string name;
  std::string version;

  err = SplitPackageNameComponents(line, &repo, &group, &name, &version);
  if (err) return err;

  err = EnsurePackage(repo, group, name, version);
  if (err) return err;

  // Perform #PrependPath.
  std::string include_path =
      "\"" + GetPackagePath(repo, group, name, version) + "\"";
  err = DoPreAppendPath((UBYTE*)include_path.c_str());
  if (err) return err;

  // Construct the arguments for #Include. By default, the main header file name
  // is assumed to be "{name}.h".
  if (incl_args.empty()) {
    incl_args = name + ".h";
  }
  if (sign != 0 && incl_args[0] != '+' && incl_args[0] != '-') {
    incl_args = (sign > 0 ? "+ " : "- ") + incl_args;
  }

  // Perform #Include.
  err = DoInclude((UBYTE*)incl_args.c_str());
  if (err) return err;

  return 0;
}
