require 'nokogiri'
require 'open-uri'
require 'csv'

start = 0
agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36"
from = "steven@example.com"
referer = "http://www.yelp.conm

CSV.open('straw.txt', "w") do |file|
	while start <= 1340
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/straw-san-francisco?start=#{start}",
															 "User-Agent" => agent,
															 "From" => from,
															 "Referer" => referer))
		puts "scraping straw review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('gary_danko.txt', "w") do |file|
	while start <= 4500
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/gary-danko-san-francisco?start=#{start}",
															 "User-Agent" => agent,
															 "From" => from,
															 "Referer" => referer))
		puts "scraping gary danko review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('cancun.txt', "w") do |file|
	while start <= 1821
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/taqueria-canc%C3%BAn-san-francisco-5?start=#{start}",
															 "User-Agent" => agent,
															 "From" => from,
															 "Referer" => referer))
		puts "scraping cancun review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('lipo.txt', "w") do |file|
	while start <= 489
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/li-po-cocktail-lounge-san-francisco?start=#{start}",
															 "User-Agent" => agent,
															 "From" => from,
															 "Referer" => referer))
		puts "scraping lipo review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('tommys.txt', "w") do |file|
	while start <= 2414
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/tommys-joynt-san-francisco?start=#{start}",
															 "User-Agent" => agent,
															 "From" => from,
															 "Referer" => referer))
		puts "scraping tommys review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end