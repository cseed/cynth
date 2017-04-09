`timescale 1 ns / 1 ps
`default_nettype none

module tb;
   
   parameter expected = 0;
   
   reg clk;
   reg resetn;
   
   wire [31:0] retval;
   reg         start;
   wire        valid;
   f dut(.__clk(clk),
         .__resetn(resetn),
         .__retval(retval),
         .__start(start),
         .__valid(valid));
   
   initial begin
      clk = 0;
      resetn = 0;
      
      #10 resetn = 1;
      
      #10 start = 1;
      #20 start = 0;
   end
   
   always
     #5 clk = !clk;
   
   initial begin
      $monitor("%d, %b, %b, %b, %x", $time, clk, resetn, valid, retval);
   end
   
   always @(posedge clk) begin
      if (valid == 1) begin
         $finish_and_return(retval != expected);
      end
   end
   
   initial
     #10000 $finish_and_return(1);
   
endmodule
