// __nonstd void sleep(int ms);
module sleep(
             input 	  __clk,
             input 	  __resetn,
             
             input [31:0] __p_ms,
             
             input 	  __start,
             output reg   __valid,
             output reg   __idle);
   
   // default 100MHz
   parameter CLK_FREQ = 100000000;
   
   // freq 1ms
   parameter DIVISOR = CLK_FREQ / 1000;
   
   reg [31:0]             ms;
   reg [31:0]             clk_counter;
   reg [31:0]             ms_counter;
   
   always @(posedge __clk) begin
      if (!__resetn) begin
         __valid <= 0;
         __idle <= 1;
      end else begin
         if (__valid) begin
            __valid <= 0;
            __idle <= 1;
         end else if (__idle) begin
            if (__start) begin
               clk_counter <= 0;
               ms_counter <= 0;
               __idle <= 0;
               if (__p_ms == 0) begin
                 __valid <= 1;
               end
               ms <= __p_ms - 1;
            end
         end else begin
            // active
            if (clk_counter == (DIVISOR - 1)) begin
               clk_counter <= 0;
               if (ms_counter == ms)
                 __valid <= 1;
               else
                 ms_counter <= ms_counter + 1;
            end else
              clk_counter <= clk_counter + 1;
         end
      end
   end
endmodule
