# Learned from https://github.com/cliffordwolf/picorv32/blob/master/Makefile

define component_name
	$(shell awk 'FNR==2{print $$4}' $(patsubst %.smt2,%.sv,$1))
endef

SOURCES=$(wildcard *.sv)
LOGS=$(patsubst %.sv,%.log,$(SOURCES))

.PHONY: all clean failed passed

all: $(LOGS)

clean:
	rm -rf *.log *.smt2 *_check*.vcd

failed: all
	grep -R FAILED *.log

passed: all
	grep -R PASSED *.log

%.log: %_check1.log %_check2.log %_check3.log
	cat $^ > $@

%.smt2: %.sv
	yosys -v2 -p "read_verilog -formal $^" -p "read_verilog -formal $(patsubst %.sv,%_bb.v,$^)" -p "prep" -p "write_smt2 -wires $@"

# bmc
%_check1.log: %.smt2
	yosys-smtbmc -s z3 -t 30 --dump-vcd $(patsubst %.smt2,%_check1.vcd,$^) -m $(call component_name,$^) $^ 2>&1 | tee $@

# induction
%_check2.log: %.smt2
	yosys-smtbmc -s z3 -t 25 --dump-vcd $(patsubst %.smt2,%_check2.vcd,$^) -m $(call component_name,$^) -i $^ 2>&1 | tee -a $@

# cover
%_check3.log: %.smt2
	yosys-smtbmc -s z3 -t 35 --dump-vcd $(patsubst %.smt2,%_check3.vcd,$^) -m $(call component_name,$^) -c $^ 2>&1 | tee -a $@
