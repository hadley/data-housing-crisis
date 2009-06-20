require 'nokogiri'
require "FasterCSV"

def parse_file(path)
  out_path = path.gsub("htm", "csv") 
  doc = Nokogiri::HTML(open(path))
  rows = (doc/"/html/body/table[2]/tr[2]/td/table/tr/td[2]/table/tr/td/table/tr")
  
  FasterCSV.open(out_path, "w") do |csv|
    rows.each {|row| csv << parse_row(row)}
  end
end

def parse_row(row)
  (row/"td").map{|e| e.content.gsub(",", "")}
end


Dir["raw-*/*.htm"].each{|path| parse_file(path)}

