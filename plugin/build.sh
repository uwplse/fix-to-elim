git submodule init
git submodule update --init --recursive --remote
coq_makefile -f _CoqProject -o Makefile
make clean && make && make install
