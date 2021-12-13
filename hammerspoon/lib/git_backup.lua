-- From here: https://shantanugoel.com/2020/03/20/hammerspoon-backup-joplin-notes-dotfiles-git-macos/

-- Paths to the directories we want to backup
local notes_path = "/Users/shantanugoel/.config/joplin-desktop"
local dotfiles_path = "/Users/shantanugoel/dotfiles"

-- The primary sync function
local function sync(path)
  if hs.fs.chdir(path) then
    output, status = hs.execute("git add . && git commit -am update", true)
    if status then
      output, status = hs.execute("git push -u origin master", true)
      if not status then
        -- Something went wrong! No internet? Didn't add ssh keys?
        hs.notify.show("Notes Sync", "Git Push Error", output)
      end
    end
  else
    -- Check if the path exists, just in case something goes wrong or you move to a new machine etc
    hs.notify.show("Notes Sync", "Cannot change Path", notes_path)
  end
end

local function notes_sync()
  sync(notes_path)
end

local function dotfiles_sync()
  sync(dotfiles_path)
end

-- Use pathwatcher to instantly sync whenever a change is made
notes_watcher = hs.pathwatcher.new(notes_path, notes_sync):start()
dotfiles_watcher = hs.pathwatcher.new(dotfiles_path, dotfiles_sync):start()

-- If you face perf issues with pathwatcher, comment out above 2 lines
-- and uncomment below lines to use a timed sync instead
-- Time interval is set to 5 minutes (300 seconds) here but can be changed
-- as per need
-- notes_timer = hs.timer.doEvery(300, notes_sync)
-- dotfiles_timer = hs.timer.doEvery(300, dotfiles_sync)
