all:
	dune build

TESTS=minimal misc reranger stress

run-minimal:
	dune exec examples/minimal.bc

run-misc:
	dune exec examples/misc.bc

run-reranger:
	dune exec examples/reranger.bc

run-stress:
	dune exec examples/stress.bc

run-stress.exe:
	dune exec examples/stress.exe

run-cbor-explorer.exe:
	dune exec examples/cbor/cbor_of_fs.exe -- -o curdir.cbor ./
	dune exec examples/cbor/cbor_explorer.exe -- curdir.cbor
