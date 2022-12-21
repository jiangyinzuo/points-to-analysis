suffix='.c'
basename=${1%$suffix}
clang -emit-llvm -O0 -g -c $1 -o $basename'.bc'