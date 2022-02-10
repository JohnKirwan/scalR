
png <- magick::image_read('~/../Desktop/scalR_fish.png')

d <- hexSticker::sticker(png,package="scalR",h_fill="skyblue",s_width=1,w_height=1)
plot(d)