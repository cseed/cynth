
VIVADO = vivado

main.bit: main.v sleep.v write_leds.v roving.v main.xdc
	$(VIVADO) -nojournal -mode batch -source synth.tcl -log logs/synth.log

.PHONY: all
all: main.bit

.PHONY: program
program: main.bit
	$(VIVADO) -nojournal -mode batch -source program.tcl -log logs/program.log

clean:
	rm -rf .Xil
	rm -f *_webtalk.*
	rm -f logs/*.log
	rm -f main.bit
	rm -f vivado*.jou vivado*.log
	rm -f vivado_*.str
	rm -f fsm_encoding.os
