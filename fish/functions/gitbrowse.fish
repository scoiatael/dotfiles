function  gitbrowse
  open (git remote get-url origin | string replace ':' '/' | string replace 'git@' 'https://')
end
