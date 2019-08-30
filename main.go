package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/MatiasLyyra/paskal/compiler"
	"github.com/MatiasLyyra/paskal/lexer"
	"github.com/MatiasLyyra/paskal/parser"
	"llvm.org/llvm/bindings/go/llvm"
)

var outputMode string
var optLevel int
var sizeLevel int

func main() {
	flag.StringVar(&outputMode, "mode", "object", "Output mode for the compiler. Possible values object (default), asm and ir")
	flag.IntVar(&optLevel, "O", 0, "optimization level 0-3")
	flag.IntVar(&sizeLevel, "S", 0, "size level 0-3")
	flag.Parse()
	if flag.NArg() != 2 {
		flag.Usage()
		log.Fatalln(`usage: paskal [-O 0 -S 0 -mode object] input output`)
	}
	input, err := getInput(flag.Arg(0))
	if err != nil {
		log.Fatalln(err)
	}
	defer input.Close()
	output, err := getOutput(flag.Arg(1))
	if err != nil {
		log.Fatalln(err)
	}
	defer output.Close()
	llvm.InitializeAllAsmParsers()
	llvm.InitializeAllAsmPrinters()
	llvm.InitializeAllTargets()
	llvm.InitializeNativeTarget()
	execute(input, output)
}

type closableStdio struct {
	*os.File
}

func (closableStdio) Close() error {
	return nil
}

func getInput(filePath string) (io.ReadCloser, error) {
	if filePath == "" {
		return nil, fmt.Errorf("empty path")
	}
	if filePath == "-" {
		i := closableStdio{os.Stdin}
		return i, nil
	}
	return os.Open(filePath)
}

func getOutput(filePath string) (io.WriteCloser, error) {
	if filePath == "" {
		return nil, fmt.Errorf("empty path")
	}
	if filePath == "-" {
		i := closableStdio{os.Stdout}
		return i, nil
	}
	return os.OpenFile(filePath, os.O_TRUNC|os.O_CREATE|os.O_WRONLY, 0775)
}

func execute(input io.Reader, output io.Writer) {
	tokens, err := lexer.Tokenize(input)
	if err != nil {
		log.Fatalf("ERROR: %s\n", err)
		return
	}
	mod, err := parser.Module(tokens)
	if err != nil {
		log.Fatalf("Parse error: %s\n", err)
		return
	}

	c := compiler.NewCompiler(mod, optLevel, sizeLevel)
	defer c.Dispose()
	err = c.Compile()
	if err != nil {
		log.Fatalf("Compilation error: %s\n", err)
	}
	if outputMode == "ir" {
		c.Module.Dump()
	}

	if outputMode != "ir" {
		fileType := llvm.ObjectFile
		if outputMode == "asm" {
			fileType = llvm.AssemblyFile
		}
		data, err := c.Emit(fileType)
		if err != nil {
			log.Fatalln(err)
		}
		_, err = output.Write(data)
		if err != nil {
			log.Fatalln(err)
		}
	}
}
