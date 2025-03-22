const r = @cImport({
    @cInclude("raylib.h");
    @cInclude("raymath.h");
});

var screenWidth: i32 = 800;
var screenHeight: i32 = 600;

const std = @import("std");

const bytImgBricks = @embedFile("tekstuuri.png");
const bytImgBricks2 = @embedFile("bricks.png");
const bytImgWater = @embedFile("water.png");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    while (args.next()) |arg| {
        std.debug.print("{s}\n", .{arg});
        if (std.mem.startsWith(u8, arg, "-w=")) {
            screenWidth = try std.fmt.parseInt(i32, arg[3..], 10);
        }
        if (std.mem.startsWith(u8, arg, "-h=")) {
            screenHeight = try std.fmt.parseInt(i32, arg[3..], 10);
        }
    }

    r.InitWindow(screenWidth, screenHeight, "shaderi");
    r.SetTargetFPS(60);
    defer r.CloseWindow();
    r.BeginDrawing();
    r.ClearBackground(r.BLACK);
    r.DrawText("kikkelis kokkelis", 10, 10, 20, r.WHITE);
    r.EndDrawing();

    const imgBricks = r.LoadImageFromMemory(".png", bytImgBricks, bytImgBricks.len);
    const texBricks = r.LoadTextureFromImage(imgBricks);

    const imgBricks2 = r.LoadImageFromMemory(".png", bytImgBricks2, bytImgBricks2.len);
    const texBricks2 = r.LoadTextureFromImage(imgBricks2);

    const imgWater = r.LoadImageFromMemory(".png", bytImgWater, bytImgWater.len);
    const texWater = r.LoadTextureFromImage(imgWater);

    const imgback = r.GenImageColor(screenWidth, screenHeight, r.Color{ .r = 0, .g = 0, .b = 255, .a = 255 });
    const tex = r.LoadTextureFromImage(imgback);

    const shader = r.LoadShader(0, "main.fs");

    const resolutionLoc = r.GetShaderLocation(shader, "resolution");
    const resolution = [2]f32{ @as(f32, @floatFromInt(screenWidth)), @as(f32, @floatFromInt(screenHeight)) };
    r.SetShaderValue(shader, resolutionLoc, resolution[0..], r.SHADER_UNIFORM_VEC2);

    const timeLoc = r.GetShaderLocation(shader, "u_time");

    const t0 = std.time.milliTimestamp();
    const texBrickLoc = r.GetShaderLocation(shader, "texbrick");
    const texBrickLoc2 = r.GetShaderLocation(shader, "texbrick2");
    const texWaterLoc = r.GetShaderLocation(shader, "texwater");

    const phiLoc = r.GetShaderLocation(shader, "phi");
    const thetaLoc = r.GetShaderLocation(shader, "theta");
    const playerLoc = r.GetShaderLocation(shader, "player");
    var playerPosition = [3]f32{ 0, 1, 0 };

    var playerSpeed = [3]f32{ 0, 0, 0 };
    var playerRotate: f32 = 0.0;

    var phi: f32 = 0.0;
    var theta: f32 = 0.0;

    r.SetMouseCursor(1);

    while (!r.WindowShouldClose()) {
        if (r.IsKeyDown(r.KEY_RIGHT)) {
            playerPosition[0] += 0.03 * std.math.cos(phi);
            playerPosition[2] += 0.03 * std.math.sin(phi);
        }
        if (r.IsKeyDown(r.KEY_LEFT)) {
            playerPosition[0] -= 0.03 * std.math.cos(phi);
            playerPosition[2] -= 0.03 * std.math.sin(phi);
        }
        if (r.IsKeyDown(r.KEY_UP)) {
            playerPosition[1] += 0.03;
        }
        if (r.IsKeyDown(r.KEY_DOWN)) {
            playerPosition[1] -= 0.03;
        }
        if (r.IsKeyDown(r.KEY_W)) {
            playerPosition[0] += 0.03 * std.math.cos(-phi + std.math.pi / 2.0);
            playerPosition[2] += 0.03 * std.math.sin(-phi + std.math.pi / 2.0);
        }
        if (r.IsKeyDown(r.KEY_S)) {
            playerPosition[0] -= 0.03 * std.math.cos(-phi + std.math.pi / 2.0);
            playerPosition[2] -= 0.03 * std.math.sin(-phi + std.math.pi / 2.0);
        }

        if (r.IsKeyDown(r.KEY_D)) {
            phi += 0.01;
        }
        if (r.IsKeyDown(r.KEY_A)) {
            phi -= 0.01;
        }
        if (r.IsKeyDown(r.KEY_Q)) {
            theta -= 0.01;
        }
        if (r.IsKeyDown(r.KEY_Z)) {
            theta += 0.01;
        }

        //---- Clicking "go there" -----
        if (r.IsMouseButtonPressed(r.MOUSE_BUTTON_LEFT)) {
            const mx = r.GetMouseX();
            const my = r.GetMouseY();

            std.debug.print("mouse {},{}\n", .{ mx, my });
            if (my < @divTrunc(screenHeight, 3)) { //want to look up
                playerSpeed[1] += 0.1 * @as(f32, @floatFromInt(screenHeight - my * 3)) / @as(f32, @floatFromInt(screenHeight));
            } else {
                if (@divTrunc(screenHeight * 2, 3) < my) {
                    playerSpeed[1] += -0.1 * (@as(f32, @floatFromInt(my)) - @as(f32, @floatFromInt(screenHeight)) * 2 / 3) / @as(f32, @floatFromInt(screenHeight));
                }
            }

            if (mx < @divTrunc(screenWidth, 3)) {
                playerRotate += -0.08333 * (@as(f32, @floatFromInt(screenWidth)) / 3 - @as(f32, @floatFromInt(mx))) / (@as(f32, @floatFromInt(screenWidth)) / 3);
            }
            if (@divTrunc(screenWidth * 2, 3) < mx) {
                playerRotate += 0.08333 * ((@as(f32, @floatFromInt(mx)) - @as(f32, @floatFromInt(screenWidth)) * 2 / 3)) / (@as(f32, @floatFromInt(screenWidth)) / 3);
            }

            if ((@abs(@divFloor(screenWidth, 2) - mx) < @divFloor(screenWidth, 4)) and (@abs(@divFloor(screenHeight, 2) - my) < @divFloor(screenHeight, 4))) {
                playerSpeed[0] += 0.03 * std.math.cos(-phi + std.math.pi / 2.0);
                playerSpeed[2] += 0.03 * std.math.sin(-phi + std.math.pi / 2.0);
                //strafe if just slightly off
                const strafe: f32 = @as(f32, @floatFromInt(@divFloor(screenWidth, 2) - mx)) / @as(f32, @floatFromInt(@divFloor(screenWidth, 2)));

                playerSpeed[0] -= 0.5 * std.math.cos(-phi) * strafe;
                playerSpeed[2] -= 0.5 * std.math.sin(-phi) * strafe;
            }
        }

        playerPosition[0] += playerSpeed[0]; //delta t?
        playerPosition[1] += playerSpeed[1];
        playerPosition[2] += playerSpeed[2];

        phi += playerRotate;

        playerSpeed[0] *= 0.99; //slowdown
        playerSpeed[1] *= 0.99;
        playerSpeed[2] *= 0.99;
        playerRotate *= 0.8;

        // Draw
        r.BeginDrawing();

        r.ClearBackground(r.RAYWHITE);

        r.BeginShaderMode(shader);
        //-------
        var timevalue: f32 = @as(f32, @floatFromInt(std.time.milliTimestamp() - t0)) / 1000;
        r.SetShaderValue(shader, timeLoc, &timevalue, r.SHADER_UNIFORM_FLOAT);
        r.SetShaderValue(shader, playerLoc, playerPosition[0..], r.SHADER_UNIFORM_VEC3);
        r.SetShaderValue(shader, phiLoc, &phi, r.SHADER_UNIFORM_FLOAT);
        r.SetShaderValue(shader, thetaLoc, &theta, r.SHADER_UNIFORM_FLOAT);

        r.SetShaderValueTexture(shader, texBrickLoc, texBricks);
        r.SetShaderValueTexture(shader, texBrickLoc2, texBricks2);

        r.SetShaderValueTexture(shader, texWaterLoc, texWater);
        r.DrawTexture(tex, 0, 0, r.WHITE);
        r.EndShaderMode();
        r.EndDrawing();
    }

    //--------------------------------------------------------------------------------------
    r.UnloadShader(shader); // Unload shader
    r.UnloadTexture(texBricks); // Unload texture
    r.UnloadTexture(texBricks2); // Unload texture
    r.UnloadTexture(texWater); // Unload texture
    r.UnloadTexture(tex);
}
