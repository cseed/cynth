module f(
  input __clk,
  input __resetn,

  // write_leds interface
  output reg [3:0] __p_c_write_leds,
  output reg __start_write_leds,
  input __idle_write_leds,
  input __valid_write_leds,


  // sleep interface
  output reg [31:0] __p_ms_sleep,
  output reg __start_sleep,
  input __idle_sleep,
  input __valid_sleep,

  input __start,
  output __idle,
  output __valid);
  reg [31:0] __state;
  reg [3:0] c;
  reg [0:0] dir;
  assign __idle = (__state == 39);
  assign __valid = (__state == 40);
  always @(posedge __clk) begin
    if (!__resetn) begin
      __state <= 39;
      __start_write_leds <= 0;
      __start_sleep <= 0;
    end else begin
      case (__state)
        39 : begin
          if (__start)
            __state <= 0;
        end
        0 : begin : __block_gen17
          reg [31:0] __t_gen18;
          __t_gen18 = 32'd1;
          c <= __t_gen18[3:0];
          __state <= 1;
        end
        1 : begin : __block_gen19
          reg [31:0] __t_gen20;
          __t_gen20 = 32'd1;
          dir <= __t_gen20[0:0];
          __state <= 37;
        end
        37 : begin : __block_gen21
          __state <= 34;
        end
        34 : begin : __block_gen22
          if ((32'd1) != (32'd0))
            __state <= 35;
          else
            __state <= 36;
        end
        35 : begin : __block_gen23
          __state <= 2;
        end
        2 : begin : __block_gen24
          if ((dir) != (1'd0))
            __state <= 3;
          else
            __state <= 4;
        end
        3 : begin : __block_gen25
          __state <= 5;
        end
        5 : begin : __block_gen26
          if (({31'd0, ($signed({28'd0, (c)}) == $signed(32'd8))}) != (32'd0))
            __state <= 6;
          else
            __state <= 7;
        end
        7 : begin : __block_gen27
          __state <= 8;
        end
        8 : begin : __block_gen28
          __state <= 4;
        end
        6 : begin : __block_gen29
          __state <= 9;
        end
        9 : begin : __block_gen30
          reg [31:0] __t_gen31;
          __t_gen31 = 32'd0;
          dir <= __t_gen31[0:0];
          __state <= 21;
        end
        21 : begin : __block_gen32
          __state <= 22;
        end
        4 : begin : __block_gen33
          __state <= 13;
        end
        13 : begin : __block_gen34
          if (({31'd0, (dir)}) != (32'd0))
            __state <= 14;
          else
            __state <= 15;
        end
        15 : begin : __block_gen35
          __state <= 16;
        end
        16 : begin : __block_gen36
          if (({31'd0, ($signed({28'd0, (c)}) == $signed(32'd1))}) != (32'd0))
            __state <= 17;
          else
            __state <= 18;
        end
        18 : begin : __block_gen37
          __state <= 19;
        end
        19 : begin : __block_gen38
          __state <= 14;
        end
        17 : begin : __block_gen39
          __state <= 20;
        end
        20 : begin : __block_gen40
          reg [31:0] __t_gen41;
          __t_gen41 = 32'd1;
          dir <= __t_gen41[0:0];
          __state <= 14;
        end
        14 : begin : __block_gen42
          __state <= 22;
        end
        22 : begin : __block_gen43
          __state <= 25;
        end
        25 : begin : __block_gen44
          if ((dir) != (1'd0))
            __state <= 26;
          else
            __state <= 27;
        end
        26 : begin : __block_gen45
          __state <= 23;
        end
        23 : begin : __block_gen46
          reg [31:0] __t_gen47;
          reg [31:0] __t_gen48;
          __t_gen48 = {28'd0, (c)};
          __t_gen47 = {__t_gen48[30:0], 1'd0};
          c <= __t_gen47[3:0];
          __state <= 28;
        end
        28 : begin : __block_gen49
          __state <= 29;
        end
        27 : begin : __block_gen50
          __state <= 24;
        end
        24 : begin : __block_gen51
          reg [31:0] __t_gen52;
          reg [31:0] __t_gen53;
          __t_gen53 = {28'd0, (c)};
          __t_gen52 = {{1{__t_gen53[31]}}, __t_gen53[31:1]};
          c <= __t_gen52[3:0];
          __state <= 29;
        end
        29 : begin : __block_gen54
          __state <= 31;
        end
        31 : begin : __block_gen55
          __p_c_write_leds <= c;
          __start_write_leds <= 1;
          __state <= 41;
        end
        41 : begin
          __start_write_leds <= 0;
          if (__valid_write_leds) begin
            __state <= 33;
          end
        end
        33 : begin : __block_gen56
          __p_ms_sleep <= 32'd1000;
          __start_sleep <= 1;
          __state <= 42;
        end
        42 : begin
          __start_sleep <= 0;
          if (__valid_sleep) begin
            __state <= 38;
          end
        end
        38 : begin : __block_gen57
          __state <= 37;
        end
        36 : begin : __block_gen58
          __state <= 40;
        end
        40 : begin
          __state <= 39;
        end
      endcase
    end
  end

endmodule
