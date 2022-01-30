-- Window movement
hs.hotkey.bind({"cmd", "shift"}, "h", function()
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

hs.hotkey.bind({"cmd", "shift"}, "l", function()
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

hs.hotkey.bind({"cmd", "shift"}, "k", function()
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

hs.hotkey.bind({"cmd", "shift"}, "j", function()
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

hs.hotkey.bind({"cmd", "shift"}, "c", function()
      local win = hs.window.focusedWindow()
      local screen = win:screen()

      win:centerOnScreen(screen)
end)

-- Window switch
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

-- Menubar items
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

-- Hot reload
hs.hotkey.bind({"cmd", "shift"}, "r", function()
      hs.reload()
end)
hs.alert.show("Config loaded")
