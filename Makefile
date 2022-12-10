examples/%/input: .cookie
	mkdir -p $$(dirname $@)
	curl https://adventofcode.com/2022/day/$*/input -H"Cookie: $$(cat .cookie)" -sSfo $@

# Use bash to expand this splat so I don't 
ALL_INPUTS=$(shell bash -c 'echo examples/{1..25}/input')

.PHONY:
inputs: $(ALL_INPUTS)
