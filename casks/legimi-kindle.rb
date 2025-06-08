cask "legimi-kindle" do
  version "0.1.0"
  sha256 "d27358d1114791de74a62c2b317ac793a7cfdc3fb8f49ea8dfaf2d94b377330d"

  url "https://files.legimi.com/static/installers/legimi-kindle.dmg"
  name "Legimi"
  desc "Synchronize your Kindle e-book reader with Legimi library"
  homepage "https://www.legimi.pl/"

  livecheck do
    url :url
    strategy :header_match
  end

  app "Legimi for Kindle.app"

  zap trash: [
    "~/Library/Caches/com.plausiblelabs.crashreporter.data/com.legimi.kindle/*",
    "~/Library/Preferences/com.legimi.kindle.plist",
    "~/Library/HTTPStorages/com.legimi.kindle/*",
    "~/Library/Application Support/com.legimi.kindle/*",
    "~/Library/Caches/com.legimi.kindle/*",
  ]
end
