# Make a label
marr_label <- c('very unhappy',
                'unhappy',
                'average',
                'happy',
                'very happy')                   

# Label a new vector
marriage <- factor(affairs$ratemarr,
                   labels = marr_label)

# Prop table
prop.table(table(marriage))

for(i in 1:length(df)){
	print(i)
}

print(i
