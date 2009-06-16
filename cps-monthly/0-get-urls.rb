require 'nokogiri'

doc = Nokogiri::HTML(open("0-cps.html"))
urls = (doc/"a").map{|link| link.attribute("href")}.grep(/.gz/).grep(/basic/).grep(/pub\./)

File.open("0-urls.txt", "w") do |f|
  urls.each{|url| f.write url.to_str + "\n"}
end
