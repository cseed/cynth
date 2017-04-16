module main(
	    input 	 sys_clk,
	    output [3:0] led);
   
   wire 	   clk = sys_clk;
   
   // reset pulse on startup
   reg [3:0] 	   reset_cnt = 0;
   wire 	   resetn = &reset_cnt;
   always @(posedge clk)
     if (!resetn)
       reset_cnt <= reset_cnt + 1;
   
   wire [31:0] 	   __p_ms_sleep;
   wire 	   __start_sleep;
   wire 	   __idle_sleep;
   wire 	   __valid_sleep;
   sleep __inst_sleep(.__clk(clk),
		      .__resetn(resetn),
		      .__p_ms(__p_ms_sleep),
		      .__start(__start_sleep),
		      .__idle(__idle_sleep),
		      .__valid(__valid_sleep));
   
   wire [3:0] 	   __p_c_write_leds;
   wire 	   __start_write_leds;
   wire 	   __idle_write_leds;
   wire 	   __valid_write_leds;
   write_leds __inst_write_leds(.__clk(clk),
				.__resetn(resetn),
				.__p_c(__p_c_write_leds),
				.leds(led),
				.__start(__start_write_leds),
				.__idle(__idle_write_leds),
				.__valid(__valid_write_leds));
   
   wire 	   idle_f;
   wire 	   valid_f;
   f f_inst(.__clk(clk),
	    .__resetn(resetn),
	    
	    .__p_c_write_leds(__p_c_write_leds),
	    .__start_write_leds(__start_write_leds),
	    .__idle_write_leds(__idle_write_leds),
	    .__valid_write_leds(__valid_write_leds),
	    
	    .__p_ms_sleep(__p_ms_sleep),
	    .__start_sleep(__start_sleep),
	    .__idle_sleep(__idle_sleep),
	    .__valid_sleep(__valid_sleep),
	    
	    .__start(1),
	    .__idle(idle_f),
	    .__valid(valid_f));
endmodule
