#!/bin/bash

base_dir="$(pwd)"
install_dir="$base_dir/install"

proto_version="0.0"

if command -v protoc &> /dev/null
then
  proto_version=$(protoc --version | awk '{print $2}')
fi

proto_major=$(echo $proto_version | cut -f1 -d.)
proto_minor=$(echo $proto_version | cut -f2 -d.)

if [[ $proto_major -ge 3 && $proto_minor -ge 3 ]]
then
  echo "Installed protoc meets requirements"
elif [[ -e $install_dir/bin/protoc ]]
then
  echo "Local install of protoc found"
else
  echo "Installed protoc does not meet requirements"
  echo "Installing protoc 3.3.0 locally"
  mkdir -p $install_dir
  cd $install_dir
  wget https://github.com/protocolbuffers/protobuf/releases/download/v3.3.0/protobuf-java-3.3.0.zip
  unzip protobuf-java-3.3.0.zip
  rm protobuf-java-3.3.0.zip
  cd protobuf-3.3.0
  ./configure --prefix=$install_dir
  make
  make check
  make install
fi

export PATH=$install_dir/bin:$PATH
export LD_LIBRARY_PATH=$install_dir/lib:$LD_LIBRARY_PATH

