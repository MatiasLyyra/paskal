package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/exec"

	"github.com/MatiasLyyra/paskal/compiler"

	"llvm.org/llvm/bindings/go/llvm"

	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"
)

const maxFileSize = 1024 * 1024

var lliPath string

func main() {
	var exists bool
	lliPath, exists = os.LookupEnv("LLI_PATH")
	if exists {
		fmt.Printf("%s\n", lliPath)
	}
	_, err := exec.Command(lliPath, "--version").Output()
	if err != nil || !exists {
		fmt.Fprintf(os.Stderr, "lli (%s) could not be found\n", lliPath)
		return
	}
	llvm.InitializeNativeTarget()
	http.HandleFunc("/execute", execute)
	log.Fatalln(http.ListenAndServe(":8080", nil))
}

func execute(response http.ResponseWriter, request *http.Request) {
	responseBody := http.MaxBytesReader(response, request.Body, maxFileSize)
	tokens, err := lexer.Tokenize(responseBody)
	if err != nil {
		responseBody.Close()
		response.WriteHeader(400)
		response.Write([]byte(fmt.Sprintf("Error reading request: %s", err)))
		return
	}
	responseBody.Close()
	ast, err := parser.Module(tokens)
	if err != nil {
		responseBody.Close()
		response.WriteHeader(400)
		response.Write([]byte(fmt.Sprintf("Error while parsing request: %s", err)))
		return
	}

	c := compiler.NewCompiler(ast, 3, 3)
	defer c.Dispose()
	err = c.Compile()
	if err != nil {
		responseBody.Close()
		response.WriteHeader(400)
		response.Write([]byte(fmt.Sprintf("Error while compiling: %s", err)))
		return
	}
	if err != nil {
		response.WriteHeader(400)
		response.Write([]byte(fmt.Sprintf("Error while executing request: %s", err)))
		return
	}
	temp, err := ioutil.TempFile("/tmp", "code_*.ll")
	defer os.Remove(temp.Name())
	llvm.WriteBitcodeToFile(c.Module, temp)
	fmt.Println(temp.Name())
	buf := bytes.NewBufferString("")
	lliCmd := exec.Command(lliPath, temp.Name())
	lliCmd.Stdout = buf
	lliCmd.Stderr = buf
	lliCmd.Run()
	response.Write(buf.Bytes())
}
