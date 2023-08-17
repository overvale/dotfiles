-- Backup Menu
-- --------------------------------------------------

backupMenu = hs.menubar.new()

function lastBackupNAS ()
   local output, _, _ = hs.execute("/Users/oht/home/src/rsync-backup/last-backup-NAS.sh")
   return output
end

function lastBackupCloud ()
   local output, _, _ = hs.execute("/Users/oht/home/src/rsync-backup/last-backup-cloud.sh")
   return output
end

function backupMenuItem()
    local menuTable = {
      { title = "Latest Backups:", disabled = true },
      { title = lastBackupNAS(), disabled = true },
      { title = lastBackupCloud(), disabled = true },
    }
    return menuTable
end

backupMenu:setMenu(backupMenuItem)

backupMenu:setTitle("􀙖")
