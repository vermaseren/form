#!/bin/sh
set -eu
rootdir=$(dirname "$0")/..
prog=$(basename "$0")

# Remarks:
# - This script assumes the version tag matches with "^v[0-9].*$".
#   The version string after the prefix "v" must be in the format in
#   semantic versioning.
# - The option -C <path> affects only the dirty flag of $revision
#   (and the related variable: $version_desc) and $date,
#   if the referred Git repository is the same.

print_usage() {
  cat <<END
Usage:
  $prog [options..]

Options:
  -h, --help                  print this information
  -C <path>, --dir <path>     use <path> as the reference directory
  -r, --raw                   raw output (default)
  -c, --c                     C output
  -t, --tex                   TeX output
  -s, --shell                 shell script output
  -v, --only-version          only-version output
  -o <file>, --output <file>  output to <file>
  --date-format <format>      date format (default: '%b %e %Y' as __DATE__)
  --date-locale <locale>      date locale (default: C)
  --date-timezone <timezone>  date timezone (default: GMT0)
END
}

# Format the date given in the form of '%Y-%m-%d %H:%M:%S %z'.
#   fmt_isodate <isodate> <format>
fmt_isodate() {
  # dash (0.5.5.1) needs the following exports.
  export LC_ALL
  export TZ
  # BSD date
  date -j -f '%Y-%m-%d %H:%M:%S %z' "$1" +"$2" 2>/dev/null ||
  # GNU date
  date -d "$1" +"$2" 2>/dev/null ||
  # perl Time::Piece
  # XXX: It has problems on the time zone.
  perl -MTime::Piece <<END 2>/dev/null ||
    print Time::Piece->strptime('$1', '%Y-%m-%d %H:%M:%S %z')->strftime('$2')
END
  # Failed.
  {
    echo "$prog: error: failed to format datetime ($1)" >&2
    echo "$prog: info: GNU/BSD date not available?" >&2
    false
  }
}

refdir=$rootdir
mode=raw
output_file=
date_format='%b %e %Y'
date_locale=C
date_timezone=GMT0  # for better compatibility than UTC0

next=
for a in "$@"; do
  if [ -n "$next" ]; then
    eval "$next=\$a"
    next=
    continue
  fi
  case $a in
    -h|--help)
      print_usage
      exit
      ;;
    -C|--dir)
      next=refdir
      ;;
    -r|--raw)
      mode=raw
      ;;
    -c|--c)
      mode=c
      ;;
    -t|--tex)
      mode=tex
      ;;
    -s|--shell)
      mode=shell
      ;;
    -v|--only-version)
      mode=only-version
      ;;
    -o|--output)
      next=output_file
      ;;
    --date-format)
      next=date_format
      ;;
    --date-locale)
      next=date_locale
      ;;
    --date-timezone)
      next=date_timezone
      ;;
    *)
      echo "$prog: error: unknown option $a" >&2
      exit 1
      ;;
  esac
done
if [ -n "$next" ]; then
  echo "$prog: error: missing argument for $a" >&2
  exit 1
fi

# Same as git -C "$refdir" ... but works with git < 1.8.5.
git_C() {
  (cd "$refdir" && git "$@")
}

# Extract the version number from the latest tag, e.g.,
#   v1.0.0-xxx-yyy-zzz -> 1.0.0
version_tag=$(git_C describe --match 'v[0-9]*' --tags HEAD)
version_tmp=$(echo "$version_tag" | sed 's/^v//')
version_num=$(echo "$version_tmp" | sed 's/-.*//')

version=$version_num
version_suffix=

# Support typical pre-release versions (e.g., v1.0.0-alpha-xxx-yyy-zzz) for
#   -alpha, -alpha.1, -beta, -beta.1, -rc, -rc.1
case $version_tmp in
  *-alpha*|*-beta*|*-rc*)
    version_tmp=$(echo "$version_tmp" | sed 's/^[^-]*-//' | sed 's/-.*//')
    case $version_tmp in
      alpha*|beta*|rc*)
        version="$version-$version_tmp"
        version_suffix="-$version_tmp"
        ;;
    esac
    ;;
esac

if [ "$mode" != "only-version" ]; then
  # Get the revision identifier by git-describe.
  revision=$(git_C describe --tags --always --abbrev=7 HEAD)
  # Check if the working tree is dirty.
  git_C update-index -q --refresh
  if git_C diff-index --quiet HEAD .; then
    # If the working tree is not dirty, use the latest commit date.
    isodate=$(git_C log -1 --pretty=%ci .)
    date=$(LC_ALL="$date_locale" TZ="$date_timezone" fmt_isodate "$isodate" "$date_format")
  else
    # If the working tree is dirty, suffix "-dirty" to the revision identifier
    # and use the current date time.
    revision="$revision-dirty"
    date=$(LC_ALL="$date_locale" TZ="$date_timezone" date +"$date_format")
  fi
  # Version description.
  # Examples: "4.3.0", "v4.3.0-1-g7c9706c"
  if [ "v$version" = "$revision" ]; then
    version_desc=$version
  else
    version_desc=$revision
  fi
  # Extract MAJOR.MINOR.PATCH from the version number.
  major_version=$(expr "$version_num" : '\([0-9]\+\)' || :)
  version_num=$(expr "$version_num" : '[0-9]\+\.\?\(.*\)' || :)
  minor_version=$(expr "$version_num" : '\([0-9]\+\)' || :)
  version_num=$(expr "$version_num" : '[0-9]\+\.\?\(.*\)' || :)
  patch_version=$(expr "$version_num" : '\([0-9]\+\)' || :)
  [ -z "$major_version" ] && major_version=0
  [ -z "$minor_version" ] && minor_version=0
  [ -z "$patch_version" ] && patch_version=0
fi

print_versions() {
  case $mode in
    raw)
      cat <<END
$version
$version_desc
$revision
$date
$major_version
$minor_version
$patch_version
$version_suffix
END
      ;;
    c)
      cat <<END
#define REPO_VERSION        "$version"
#define REPO_VERSION_DESC   "$version_desc"
#define REPO_REVISION       "$revision"
#define REPO_DATE           "$date"
#define REPO_MAJOR_VERSION  $major_version
#define REPO_MINOR_VERSION  $minor_version
#define REPO_PATCH_VERSION  $patch_version
#define REPO_VERSION_SUFFIX "$version_suffix"
END
      ;;
    tex)
      cat <<END
\def\repoversion{$version}
\def\repoversiondesc{$version_desc}
\def\reporevision{$revision}
\def\repodate{$date}
\def\repomajorversion{$major_version}
\def\repominorversion{$minor_version}
\def\repopatchversion{$patch_version}
\def\repoversionsuffix{$version_suffix}
END
      ;;
    shell)
      cat <<END
repo_version='$version'
repo_version_desc='$version_desc'
repo_revision='$revision'
repo_date='$date'
repo_major_version='$major_version'
repo_minor_version='$minor_version'
repo_patch_version='$patch_version'
repo_version_suffix='$version_suffix'
END
      ;;
    only-version)
      echo "$version"
      ;;
    *)
      echo "$prog: internal error: unknown mode $mode" >&2
      exit 1
      ;;
  esac
}

say () {
  cat <<END
$@
END
}

if [ -z "$output_file" ]; then
  # To the standard output.
  print_versions
else
  # To the output file. Write only if any changes required.
  if [ -f "$output_file" ]; then
    out=$(print_versions)
    # NOTE: using echo instead of say at the next line may have problems
    #       for --tex. POSIX says echo should process '\\', but some shells
    #       don't without -e option. Here we shouldn't process them.
    say "$out" | cmp -s "$output_file" - || say "$out" >"$output_file"
  else
    print_versions >"$output_file"
  fi
fi
