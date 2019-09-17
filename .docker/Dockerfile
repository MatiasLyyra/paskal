FROM golang:1.12-stretch

RUN apt-get update -q && apt-get install -y cmake gcc subversion
RUN go get -d llvm.org/llvm/bindings/go/llvm
ADD ./build_llvm.sh /go/src/llvm.org/llvm/bindings/go/build_llvm.sh
RUN chmod +x /go/src/llvm.org/llvm/bindings/go/build_llvm.sh
RUN /go/src/llvm.org/llvm/bindings/go/build_llvm.sh -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON