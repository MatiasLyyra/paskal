#!/bin/sh

t="/tmp/go-cover.$$.tmp"
go test -coverprofile=$t ./... && go tool cover -html=$t -o cover.html && unlink $t