module f(
  input __clk,
  input __resetn,
  output reg [31:0] __retval,
  input __start,
  output __idle,
  output __valid);
  reg [31:0] __state;
  reg [31:0] i;
  reg [31:0] s;
  assign __idle = (__state == 11);
  assign __valid = (__state == 12);
  always @(posedge __clk) begin
    if (!__resetn) begin
      __state <= 11;
    end else begin
      case (__state)
        11 : begin
          if (__start)
            __state <= 0;
        end
        0 : begin
          i <= 32'd0;
          __state <= 1;
        end
        1 : begin
          s <= 32'd0;
          __state <= 8;
        end
        8 : begin
          __state <= 5;
        end
        5 : begin
          if (({31'd0, ($signed(i) < $signed(32'd3))}) != (32'd0))
            __state <= 6;
          else
            __state <= 7;
        end
        6 : begin
          __state <= 2;
        end
        2 : begin
          s <= (s) + (i);
          __state <= 4;
        end
        4 : begin
          i <= (i) + (32'd1);
          __state <= 9;
        end
        9 : begin
          __state <= 8;
        end
        7 : begin
          __state <= 10;
        end
        10 : begin
          __retval <= s;
          __state <= 12;
        end
        12 : begin
          __state <= 11;
        end
      endcase
    end
  end

endmodule

module top();
   
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
      $monitor("%d, %b, %b, %b, %x, %d", $time, clk, resetn, valid, retval, dut.i);
   end

   always @(posedge clk) begin
      if (valid == 1) begin
         $finish_and_return(retval != 3);
      end
   end
   
   initial
     #1000 $finish_and_return(1);
   
endmodule
