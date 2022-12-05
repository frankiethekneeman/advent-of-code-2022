examples/%/input: .cookie
	mkdir -p $$(dirname $@)
	curl https://adventofcode.com/2022/day/$*/input -H"Cookie: $$(cat .cookie)" > $@

