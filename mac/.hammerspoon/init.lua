local SUPER_SHIFT = {"cmd", "shift"}
local SUPER_CTRL = {"cmd", "ctrl"}
local SUPER_ALT = {"cmd", "alt"}

--
-- Window movement
--
hs.hotkey.bind(SUPER_SHIFT, "h", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
end)

hs.hotkey.bind(SUPER_SHIFT, "l", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x + (max.w / 2)
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
end)

hs.hotkey.bind(SUPER_SHIFT, "k", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h / 2
      win:setFrame(f)
end)

hs.hotkey.bind(SUPER_SHIFT, "j", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y + (max.h / 2)
      f.w = max.w
      f.h = max.h / 2
      win:setFrame(f)
end)

hs.hotkey.bind(SUPER_SHIFT, "c", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:centerOnScreen(screen)
end)

--
-- Window switch
--
hs.hotkey.bind({"cmd"}, "h", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:focusWindowWest()
end)

hs.hotkey.bind({"cmd"}, "l", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:focusWindowEast()
end)

hs.hotkey.bind({"cmd"}, "j", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:focusWindowSouth()
end)

hs.hotkey.bind({"cmd"}, "k", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:focusWindowNorth()
end)

--
-- Window size
--
hs.hotkey.bind(SUPER_SHIFT, "return", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:maximize()
end)

hs.hotkey.bind(SUPER_SHIFT, "m", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:minimize()
end)

hs.hotkey.bind(SUPER_SHIFT, "f", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:toggleFullScreen()
end)

-- increase width and height
function resize(wPercent, hPercent)
    return function()
        local win = hs.window.focusedWindow()
        local f = win:frame()
        local screen = win:screen()
        local max = screen:frame()

        wInc = wPercent * f.w
        hInc = hPercent * f.h

        f.w = f.w + wInc
        f.x = f.x - wInc / 2

        if f.x < max.x then
            f.x = max.x
        end

        f.h = f.h + hInc
        f.y = f.y - hInc / 2
        if f.y < max.y then
            f.y = max.y
        end

        win:setFrame(f)
    end
end

local window_resize_ratio = 0.15
hs.hotkey.bind(SUPER_SHIFT, '.', resize(window_resize_ratio, 0))
hs.hotkey.bind(SUPER_SHIFT, ',', resize(-window_resize_ratio, 0))
hs.hotkey.bind(SUPER_SHIFT, '=', resize(0, window_resize_ratio))
hs.hotkey.bind(SUPER_SHIFT, '-', resize(0, -window_resize_ratio))
hs.hotkey.bind(SUPER_SHIFT, ']', resize(window_resize_ratio, window_resize_ratio))
hs.hotkey.bind(SUPER_SHIFT, '[', resize(-window_resize_ratio, -window_resize_ratio))

--
-- Window visibility
--

-- this equals clicking the red close button
hs.hotkey.bind(SUPER_SHIFT, "x", function()
      local win = hs.window.focusedWindow()
      win:close()
end)


--
-- Screens
--

-- Move window between screens
hs.hotkey.bind(SUPER_CTRL, "h", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()

      win:moveOneScreenWest()
end)

hs.hotkey.bind(SUPER_CTRL, "l", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()

      win:moveOneScreenEast()
end)

-- Scale
-- Larger text (zoom-in effect)
hs.hotkey.bind(SUPER_CTRL, "=", function()
                   local screen = hs.screen.primaryScreen()
                   local cur = screen:currentMode()
                   screen:setMode(1280, 800, cur.scale, cur.freq, cur.depth)
end)

-- Smaller text (zoom-out effect)
hs.hotkey.bind(SUPER_CTRL, "-", function()
                   local screen = hs.screen.primaryScreen()
                   local cur = screen:currentMode()
                   screen:setMode(1440, 900, cur.scale, cur.freq, cur.depth)
end)

--
-- Applications
--

-- kill gracefully (close and kill)
hs.hotkey.bind(SUPER_SHIFT, "q", function()
      local win = hs.window.focusedWindow()
      win:application():kill()
end)

--
-- Lock
--
hs.hotkey.bind(SUPER_SHIFT, "escape", function()
        hs.caffeinate.lockScreen()
end)

--
-- Menubar items
--
caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    if state then
        caffeine:setTitle("AWAKE")
    else
        caffeine:setTitle("SLEEPY")
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

--
-- URLs
--

-- call iTerm with command
-- ref: https://iterm2.com/documentation-scripting.html
hs.urlevent.bind("terminal", function(eventName, params)
                     local command = params['command']
                     local dir = params['dir']

                     if dir then
                         cd_command = string.format([[
  tell current session of tab -1 of current window
    write text "cd %s"
  end tell
]], dir)
                     else
                         cd_command = ""
                     end

                     if command then
                         exec_command = string.format([[
  tell current session of tab -1 of current window
    write text "%s"
  end tell
]], command)
                     else
                         exec_command = ""
                     end

                     hs.osascript.applescript(string.format([[
activate application "iTerm"
tell application "iTerm" to run

tell application "iTerm"
  tell current window
    create tab with default profile
  end tell

  %s

  %s
end tell
]], cd_command, exec_command))
end)

--
-- Clipboard management
-- ref: https://www.hammerspoon.org/Spoons/ClipboardTool.html
--
function setupClipboardTool()
    tool = hs.loadSpoon("ClipboardTool")
    tool.show_copied_alert = false
    tool.paste_on_select = true
    tool:start()
    tool:bindHotkeys({
            toggle_clipboard = {SUPER_ALT, "p"}
    })
end

setupClipboardTool()

--
-- Hot reload
--
hs.hotkey.bind(SUPER_SHIFT, "r", function()
      hs.reload()
end)
hs.alert.show("Config loaded")
