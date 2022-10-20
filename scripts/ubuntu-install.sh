#! /bin/bash

THIS_SCRIPT_FILE_MAYBE_RELATIVE="$0"
THIS_SCRIPT_DIR_MAYBE_RELATIVE="${THIS_SCRIPT_FILE_MAYBE_RELATIVE%/*}"
THIS_SCRIPT_DIR_ABSOLUTE=`readlink -f "${THIS_SCRIPT_DIR_MAYBE_RELATIVE}"`

# Install the new directories as siblings of the avenir repo directory.
# I believe that the _opam directory created during installation must be
# in a directory that is somewhere above the avenir/synthesis directory,
# otherwise Ocaml builds will use the ~/.opam directory contents, which
# are not the desired version of the OCaml compiler.
cd ${THIS_SCRIPT_DIR_ABSOLUTE}/../..
INSTALL_DIR="${PWD}"

OPAMENV="${INSTALL_DIR}/avenir-install-details"
mkdir -p ${OPAMENV}

ubuntu_version_warning() {
    1>&2 echo "This script has only been tested on these systems:"
    1>&2 echo "    Ubuntu 18.04"
    1>&2 echo "    Ubuntu 20.04 (TODO)"
    1>&2 echo ""
    1>&2 echo "Proceed installing manually at your own risk of"
    1>&2 echo "significant time spent figuring out how to make it all"
    1>&2 echo "work, or consider getting VirtualBox and creating an"
    1>&2 echo "Ubuntu virtual machine with one of the tested versions."
}

abort_script=0

lsb_release >& /dev/null
if [ $? != 0 ]
then
    1>&2 echo "No 'lsb_release' found in your command path."
    ubuntu_version_warning
    exit 1
fi

distributor_id=`lsb_release -si`
ubuntu_release=`lsb_release -s -r`
if [ "${distributor_id}" = "Ubuntu" -a \( "${ubuntu_release}" = "18.04" -o "${ubuntu_release}" = "20.04" \) ]
then
    echo "Found distributor '${distributor_id}' release '${ubuntu_release}'.  Continuing with installation."
else
    ubuntu_version_warning
    1>&2 echo ""
    1>&2 echo "Here is what command 'lsb_release -a' shows this OS to be:"
    lsb_release -a
    exit 1
fi

# Minimum required system memory is 2 GBytes, minus a few MBytes
# because from experiments I have run on several different Ubuntu
# Linux VMs, when you configure them with 2 Gbytes of RAM, the first
# line of /proc/meminfo shows a little less than that available, I
# believe because some memory occupied by the kernel is not shown.

min_mem_MBytes=`expr 2 \* \( 1024 - 64 \)`
memtotal_KBytes=`head -n 1 /proc/meminfo | awk '{print $2;}'`
memtotal_MBytes=`expr ${memtotal_KBytes} / 1024`

if [ "${memtotal_MBytes}" -lt "${min_mem_MBytes}" ]
then
    memtotal_comment="too low"
    abort_script=1
else
    memtotal_comment="enough"
fi

echo "Minimum recommended memory to run this script: ${min_mem_MBytes} MBytes"
echo "Memory on this system from /proc/meminfo:      ${memtotal_MBytes} MBytes -> $memtotal_comment"

min_free_disk_MBytes=`expr 8 \* 1024`
free_disk_MBytes=`df --output=avail --block-size=1M . | tail -n 1`

if [ "${free_disk_MBytes}" -lt "${min_free_disk_MBytes}" ]
then
    free_disk_comment="too low"
    abort_script=1
else
    free_disk_comment="enough"
fi

echo "Minimum free disk space to run this script:    ${min_free_disk_MBytes} MBytes"
echo "Free disk space on this system from df output: ${free_disk_MBytes} MBytes -> $free_disk_comment"

if [ "${abort_script}" == 1 ]
then
    echo ""
    echo "Aborting script because system has too little RAM or free disk space"
    exit 1
fi

Z3_DIR_NAME="z3-4.8.8-x64-ubuntu-16.04"

1>&2 echo ""
1>&2 echo "Install directory: ${INSTALL_DIR}"
1>&2 echo ""
1>&2 echo "Inside that directory, this script will create new directories"
1>&2 echo "with these names:"
1>&2 echo "    _opam"
1>&2 echo "    petr4"
1>&2 echo "    ${Z3_DIR_NAME}"

if [ -e "_opam" -o -e "peter4" -o -e ${Z3_DIR_NAME} ]
then
    1>&2 echo ""
    1>&2 echo "One or more files with those names already exist."
    1>&2 echo "Remove or rename them, then try this script again."
    exit 1
fi

echo "------------------------------------------------------------"
echo "Time and disk space used before installation begins:"
set -x
date
df -h .
df -BM .

# This section for installing opam is based on instructions found
# here: https://opam.ocaml.org/doc/Install.html

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt update
sudo apt-get install --yes opam
opam --version

# As of 2022-Oct-16 when I last ran this on Ubuntu 18.04 and 20.04
# systems, the commands above installed opam version 2.1.0

opam --yes init
opam env --switch=default > ${OPAMENV}/opam-env-1.txt
eval $(opam env --switch=default)
opam --yes switch create . ocaml-base-compiler.4.09.0
opam env > ${OPAMENV}/opam-env-2.txt
eval $(opam env)
sudo apt-get install --yes m4
opam --yes install merlin dune utop core ocamlformat
opam env > ${OPAMENV}/opam-env-3.txt
eval $(opam env)
opam --yes user-setup install
sudo apt-get install --yes bubblewrap
opam --yes install menhir

# I tried 'sudo apt-get install z3' on Ubuntu 20.04 on 2022-Oct-16,
# but it install version 4.8.7 according to the output of `z3
# --version`, which is older than the version 4.8.8 recommended by the
# Avenir README.

# Instead, install a pre-built version of z3 4.8.8 as distributed by
# the Z3 developers.

# Download Z3 zip file
wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.8/z3-4.8.8-x64-ubuntu-16.04.zip
# unzip it
unzip -e z3-4.8.8-x64-ubuntu-16.04.zip
# create a link from /usr/bin/z3 to where it is installed
Z3_INSTALL_DIR="${INSTALL_DIR}/z3-4.8.8-x64-ubuntu-16.04"
sudo ln ${Z3_INSTALL_DIR}/bin/z3 /usr/bin/z3

sudo apt-get install --yes libgmp-dev
opam --yes pin add z3 https://github.com/priyasrikumar/ocaml-z3.git

# Install recommended version of petr4
cd ${INSTALL_DIR}
git clone https://github.com/cornell-netlab/petr4
cd petr4
git checkout cd556c1e2c20ccbd5b959f385cecebc43f5cfd72
PETR4_INSTALL_DIR=`pwd`

cd ${THIS_SCRIPT_DIR_ABSOLUTE}/../synthesis
opam --yes install p4pp=0.1.4
opam env > ${OPAMENV}/opam-env-4.txt
eval $(opam env)
opam --yes pin add cstruct 6.0.0
opam env > ${OPAMENV}/opam-env-5.txt
eval $(opam env)
opam --yes pin add petr4 ${PETR4_INSTALL_DIR}
opam env > ${OPAMENV}/opam-env-6.txt
eval $(opam env)

# I got the following error message attempting the next command:
# dune external-lib-deps: This subcommand is no longer implemented.

# This error occured whiel running dune version 3.4.1.  Apparently
# this 'extern-lib-deps' sub-command was removed in some version of
# dune since the Avenir install instructions were written, so skip
# this command.
# dune external-lib-deps --missing @all

# These should be the packages that need to be installed, according to
# the Avenir developers.
sudo apt-get install --yes pkg-config g++
opam --yes install async cohttp-async ipaddr shell
opam env > ${OPAMENV}/opam-env-7.txt
eval $(opam env)
sudo ldconfig

make

set +x
echo "------------------------------------------------------------"
echo "Time and disk space used when installation was complete:"
set -x
date
df -h .
df -BM .
