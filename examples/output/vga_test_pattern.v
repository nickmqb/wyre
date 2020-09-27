// Generated by Wyre compiler 0.1.0

module core(
	input clk,
	output [3:0] vga_r,
	output [3:0] vga_g,
	output [3:0] vga_b,
	output vga_hs,
	output vga_vs
);

(* keep *) reg [9:0] x = 0;
(* keep *) reg [9:0] y = 0;
(* keep *) reg [7:0] frame = 0;
(* keep *) reg vis_x = 0;
(* keep *) reg vis_y = 0;
(* keep *) reg hs = 0;
(* keep *) reg vs = 0;
wire [5:0] x_offset;
(* keep *) reg [2:0] color = 0;
wire vis;

assign x_offset = (x[5:0]) + (frame[5:0]);
assign vis = (vis_x) & (vis_y);
assign vga_r = (vis) ? ({ color[0], color[0], color[0], color[0] }) : (4'd0);
assign vga_g = (vis) ? ({ color[1], color[1], color[1], color[1] }) : (4'd0);
assign vga_b = (vis) ? ({ color[2], color[2], color[2], color[2] }) : (4'd0);
assign vga_hs = ~(hs);
assign vga_vs = ~(vs);

always @(posedge clk) begin
	if ((x) == (799)) begin
		x <= 0;
	end else begin
		x <= (x) + (1);
	end
	if ((x) == (639)) begin
		if ((y) == (524)) begin
			y <= 0;
			frame <= (frame) + (1);
		end else begin
			y <= (y) + (1);
		end
	end
	if ((x) == (0)) begin
		vis_x <= 1;
	end
	if ((x) == (640)) begin
		vis_x <= 0;
	end
	if ((x) == (656)) begin
		hs <= 1;
	end
	if ((x) == (752)) begin
		hs <= 0;
	end
	if ((y) == (0)) begin
		vis_y <= 1;
	end
	if ((y) == (480)) begin
		vis_y <= 0;
	end
	if ((y) == (490)) begin
		vs <= 1;
	end
	if ((y) == (492)) begin
		vs <= 0;
	end
	color <= { frame[7], y[5], x_offset[5] };
end

endmodule