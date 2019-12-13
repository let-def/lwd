all:
	dune build

TESTS=misc reranger stress

run-misc:
	dune exec examples/misc.bc

run-reranger:
	dune exec examples/reranger.bc

run-stress:
	dune exec examples/stress.bc

run-stress.exe:
	dune exec examples/stress.exe

run-cbor-explorer.exe:
	rm curdir.cbor || true
	dune exec examples/cbor/cbor_of_fs.exe -- -o curdir.cbor ./
	dune exec examples/cbor/cbor_explorer.exe -- curdir.cbor
