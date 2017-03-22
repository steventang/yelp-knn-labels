require 'nokogiri'
require 'open-uri'
require 'csv'

start = 0

CSV.open('straw.txt', "w") do |file|
	while start <= 1340
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/straw-san-francisco?start=#{start}",
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
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
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
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
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
		puts "scraping cancun review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('state_bird.txt', "w") do |file|
	while start <= 2027
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/state-bird-provisions-san-francisco?start=#{start}",
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
		puts "scraping state bird review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('alba_rays.txt', "w") do |file|
	while start <= 58
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/alba-rays-san-francisco?start=#{start}",
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
		puts "scraping alba rays review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end

start = 0

CSV.open('lipo.txt', "w") do |file|
	while start <= 489
		page = Nokogiri::HTML(open("https://www.yelp.com/biz/li-po-cocktail-lounge-san-francisco?start=#{start}",
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
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
															 "User-Agent" => "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36",
															 "From" => "stang12290@gmail.com",
															 "Referer" => "http://www.yelp.com"))
		puts "scraping tommys review #{start} - #{start + 20}"
		file << page.xpath("//div[contains(@class,'review-content')]/p/text()")
		sleep 0.05
		start += 20
	end
end