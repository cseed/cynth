// __nonstd void write_leds(__unsigned4 c);
module write_leds(
		  input 	   __clk,
		  input 	   __resetn,
		  
		  input [3:0] 	   __p_c,
		  output reg [3:0] leds,
		  
		  input 	   __start,
		  output reg 	   __valid,
		  output reg 	   __idle);
   
   always @(posedge __clk) begin
      if (!__resetn) begin
	 __valid <= 0;
	 __idle <= 1;
	 leds <= 0;
      end else begin
	 if (__valid) begin
	    __valid <= 0;
	    __idle <= 1;
	 end else if (__idle) begin
	    if (__start) begin
	       __valid <= 1;
	       __idle <= 0;
	       leds <= __p_c;
	    end
	 end
      end
   end
   
endmodule // write_leds
