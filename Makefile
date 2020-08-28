all:
	dune build

TESTS=minimal misc reranger stress

$(TESTS):
	dune build examples/$@.bc

run-minimal:
	dune exec examples/minimal.bc

run-misc:
	dune exec examples/misc.bc

run-reranger:
	dune exec examples/reranger.bc

run-stress:
	dune exec examples/stress.bc

run-pretty:
	dune exec examples/pretty.bc

run-pretty-lambda:
	dune exec examples/pretty_lambda.bc

run-stress.exe:
	dune exec examples/stress.exe

run-cbor-explorer.exe:
	rm curdir.cbor || true
	dune exec examples/cbor/cbor_of_fs.exe -- -o curdir.cbor ./
	dune exec examples/cbor/cbor_explorer.exe -- curdir.cbor
