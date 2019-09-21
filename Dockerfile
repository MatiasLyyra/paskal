FROM matiaslyyra/go-llvm-bindings:1.0

RUN mkdir -p /go/src/github.com/MatiasLyyra/paskal /app
WORKDIR /app
ADD . /go/src/github.com/MatiasLyyra/paskal
RUN go build /go/src/github.com/MatiasLyyra/paskal/server
RUN chmod +x /go/src/llvm.org/llvm/bindings/go/llvm/workdir/llvm_build/bin/lli
ENV LLI_PATH /go/src/llvm.org/llvm/bindings/go/llvm/workdir/llvm_build/bin/lli
EXPOSE 8080
ENTRYPOINT [ "./server" ]