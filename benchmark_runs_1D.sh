#!/usr/bin/env bash
set -euo pipefail

repo_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
cd "$repo_dir"

input_file=${1:-models/inputChemistry_model_2025-09-11h09-55-22.txt}
if [[ "$input_file" != /* ]]; then
	input_file="$repo_dir/$input_file"
fi
if [[ ! -f "$input_file" ]]; then
	echo "Chemistry input not found: $input_file" >&2
	exit 1
fi

read_parameter() {
	local key=$1
	awk -F= -v key="$key" '
		{
			sub(/!.*/, "")
			name = $1
			gsub(/^[[:space:]]+|[[:space:]]+$/, "", name)
			if (name == key) {
				value = $2
				gsub(/^[[:space:]]+|[[:space:]]+$/, "", value)
				print value
				found = 1
				exit
			}
		}
		END { if (!found) exit 1 }
	' "$input_file"
}

ISTELLAR=$(read_parameter ISTELLAR) || {
	echo "ISTELLAR is missing from $input_file" >&2
	exit 1
}
IBIN=$(read_parameter IBIN) || {
	echo "IBIN is missing from $input_file" >&2
	exit 1
}
if [[ ! "$ISTELLAR" =~ ^[01]$ || ! "$IBIN" =~ ^[01]$ ]]; then
	echo "ISTELLAR and IBIN must each be 0 or 1 (got $ISTELLAR and $IBIN)." >&2
	exit 1
fi

build_args=()
if [[ "$ISTELLAR" == 1 ]]; then
	build_args+=(-IP)
fi
if [[ "$IBIN" == 1 ]]; then
	TBIN=$(read_parameter TBIN) || {
		echo "TBIN is required when IBIN=1 in $input_file" >&2
		exit 1
	}
	if [[ ! "$TBIN" =~ ^(4000|6000|10000)$ ]]; then
		echo "Unsupported TBIN=$TBIN; available AP networks are 4000, 6000, and 10000 K." >&2
		exit 1
	fi
	build_args+=("-AP=$TBIN")
fi

echo "Input: $input_file"
echo "ISTELLAR=$ISTELLAR, IBIN=$IBIN"
if ((${#build_args[@]})); then
	printf 'Required UMIST network build: ./build_UMIST.sh umist_rate22'
	printf ' %q' "${build_args[@]}"
	printf '\n'
	echo "The current 1D runner does not set the stellar/binary flux variables required by these rates." >&2
	echo "Refusing to run until CSE_run_krome_1D wires those fluxes to the input parameters." >&2
	exit 1
fi

echo "Building UMIST rate22 without -IP/-AP (both photon switches are off)."
./build_UMIST.sh umist_rate22
./make_CSEkrome.sh 1D
(cd krome/build && ./run_CSE_krome_1D "$input_file")