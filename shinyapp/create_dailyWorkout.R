
library(stringr)
create_yearPlan = function(){
  year = str_split(Sys.Date(),'-')[[1]][1]
  phase = c(rep('build',10),
            rep('light',11),
            rep('cardio',4),
            rep('off/rehab',6))
  phase = sample(phase, size = length(phase),replace = F)
  weekByweek = vector()
  for(a in phase){
    if(a == 'build'){
      weekByweek = append(weekByweek,rep('build',2))
    } else if(a == 'light'){
      weekByweek = append(weekByweek, rep('light',2))
    } else if(a == 'cardio'){
      weekByweek = append(weekByweek, 'cardio')
    } else{
      weekByweek = append(weekByweek, 'off/rehab')
    }
  }
  # decide what day it is
  day = c(rep('push',2),rep('pull',2),rep('legs',2),'off')
  day = sample(day, size = length(day),replace = F)
  ## generate daily phase/day
  chartDat = data.frame(cbind(
    rep(weekByweek, each = 7),
    rep(day,52)
  ))
  names(chartDat) = c('phase','day')
  beg_date = paste(year,"-01-01",sep='')
  end_date = paste(year,"-12-31",sep='')
  chartDat$date = seq(as.Date("2020-01-01"),
                      as.Date("2020-12-31"),by="days")[1:nrow(chartDat)]
  
  return(chartDat)
}

if(str_split(Sys.Date(),'-')[[1]][1] != str_split(read.csv('phaseDay.csv')$date[1],"-")[[1]][1]){
  ydat = create_yearPlan()
  write.csv(ydat,'phaseDay.csv')
}

# load in exercise list
exlist = read.csv('exercise_list.csv')
phaseday = read.csv('phaseDay.csv')

cardio_minutes = c(10,15,20,25,30)
ab_reps = c(15,20,25,30,25)
build_sets = c("3x10", "4x7","5x4")
light_sets = c("4x10", "3x20","4x25")
tmpDay = phaseday[phaseday$date == Sys.Date(),]
what_phase = tmpDay$phase
what_day = tmpDay$day
#phases: build, light, cardio, off/rehab
#day: legs, off, pull, push

separate_df = data.frame(
  exercise = c('-----','-----','-----'),
  set = c('-----','-----','-----')
)


# off day workout
create_off = function(){
  curDay_workout = data.frame(
    exercise = c('Off.'),
    set = c('Enjoy the day')
  )
  return(curDay_workout)
}

# cardio day workout
create_cardio = function(){
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 3), ][,1],
    set = c(sample(cardio_minutes,1),sample(cardio_minutes,1),
            sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat_card2,tmpdat_abs2
  ))
}

# off/rehab day
create_offRehab = function(){
  tmpdat = data.frame(
    exercise = c('deep stretch','band work', 'yoga','abs of choice'),
    set = c(' ',' ',' ',' ')
  )
  return(tmpdat)
}


## build legs
create_buildLegs = function(){
  tmpdat_legs = subset(exlist, category == 'legs')
  tmpdat_compound = subset(tmpdat_legs, group == 'compound')
  tmpdat_lower_back = subset(tmpdat_legs, group == 'lower_back')
  tmpdat_quad = subset(tmpdat_legs, group == 'quad')
  tmpdat_ham = subset(tmpdat_legs, group == 'hamstring')
  tmpdat_calf = subset(tmpdat_legs, group == 'calves')
  tmpdat_glute = subset(tmpdat_legs, group == 'glute')
  if(sample(c(1,2),1) == 1){
    tmpdat2 = data.frame(rbind(
      tmpdat_compound[sample(nrow(tmpdat_compound),4), ],
      tmpdat_lower_back[sample(nrow(tmpdat_lower_back),2),],
      tmpdat_quad,
      tmpdat_ham,
      tmpdat_calf,
      tmpdat_glute
    ))
  } else{
    tmpdat2 = data.frame(rbind(
      tmpdat_compound[sample(nrow(tmpdat_compound),4), ],
      tmpdat_lower_back[sample(nrow(tmpdat_lower_back),2),],
      tmpdat_quad,
      tmpdat_ham,
      tmpdat_calf
    ))
  }
  rows = sample(nrow(tmpdat2))
  tmpdat2 = tmpdat2[rows,]
  tmpdat2 = data.frame(
    exercise = tmpdat2$exercise
  )
  tmpdat2$set = NA
  for(za in c(1:nrow(tmpdat2))){
    tmpdat2$set[za] = sample(build_sets,1)
  }
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 1), ][,1],
    set = c(sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat2,tmpdat_card2,tmpdat_abs2
  ))
}

## light legs
create_lightLegs = function(){
  tmpdat_legs = subset(exlist, category == 'legs')
  tmpdat_compound = subset(tmpdat_legs, group == 'compound')
  tmpdat_lower_back = subset(tmpdat_legs, group == 'lower_back')
  tmpdat_quad = subset(tmpdat_legs, group == 'quad')
  tmpdat_ham = subset(tmpdat_legs, group == 'hamstring')
  tmpdat_calf = subset(tmpdat_legs, group == 'calves')
  tmpdat_glute = subset(tmpdat_legs, group == 'glute')
  if(sample(c(1,2),1) == 1){
    tmpdat2 = data.frame(rbind(
      tmpdat_compound[sample(nrow(tmpdat_compound),4), ],
      tmpdat_lower_back[sample(nrow(tmpdat_lower_back),2),],
      tmpdat_quad,
      tmpdat_ham,
      tmpdat_calf,
      tmpdat_glute
    ))
  } else{
    tmpdat2 = data.frame(rbind(
      tmpdat_compound[sample(nrow(tmpdat_compound),4), ],
      tmpdat_lower_back[sample(nrow(tmpdat_lower_back),2),],
      tmpdat_quad,
      tmpdat_ham,
      tmpdat_calf
    ))
  }
  rows = sample(nrow(tmpdat2))
  tmpdat2 = tmpdat2[rows,]
  tmpdat2 = data.frame(
    exercise = tmpdat2$exercise
  )
  tmpdat2$set = NA
  for(za in c(1:nrow(tmpdat2))){
    tmpdat2$set[za] = sample(light_sets,1)
  }
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 1), ][,1],
    set = c(sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat2,tmpdat_card2,tmpdat_abs2
  ))
}

## build pull
create_buildpull = function(){
  tmpdat_pull = subset(exlist, category == 'pull')
  tmpdat_compound = subset(tmpdat_pull, group == 'compound')
  tmpdat_upper_back = subset(tmpdat_pull, group == 'upper_back')
  tmpdat_bicep = subset(tmpdat_pull, group == 'bicep')
  tmpdat_traps = subset(tmpdat_pull, group == 'traps')
  tmpdat2 = data.frame(rbind(
    tmpdat_compound[sample(nrow(tmpdat_compound),5), ],
    tmpdat_upper_back[sample(nrow(tmpdat_upper_back),2), ],
    tmpdat_bicep[sample(nrow(tmpdat_bicep),4), ],
    tmpdat_traps[sample(nrow(tmpdat_traps),2), ]
  ))
  rows = sample(nrow(tmpdat2))
  tmpdat2 = tmpdat2[rows,]
  tmpdat2 = data.frame(
    exercise = tmpdat2$exercise
  )
  tmpdat2$set = NA
  for(za in c(1:nrow(tmpdat2))){
    tmpdat2$set[za] = sample(build_sets,1)
  }
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 1), ][,1],
    set = c(sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat2,tmpdat_card2,tmpdat_abs2
  ))
}


## light pull
create_lightpull = function(){
  tmpdat_pull = subset(exlist, category == 'pull')
  tmpdat_compound = subset(tmpdat_pull, group == 'compound')
  tmpdat_upper_back = subset(tmpdat_pull, group == 'upper_back')
  tmpdat_bicep = subset(tmpdat_pull, group == 'bicep')
  tmpdat_traps = subset(tmpdat_pull, group == 'traps')
  tmpdat2 = data.frame(rbind(
    tmpdat_compound[sample(nrow(tmpdat_compound),5), ],
    tmpdat_upper_back[sample(nrow(tmpdat_upper_back),2), ],
    tmpdat_bicep[sample(nrow(tmpdat_bicep),4), ],
    tmpdat_traps[sample(nrow(tmpdat_traps),2), ]
  ))
  rows = sample(nrow(tmpdat2))
  tmpdat2 = tmpdat2[rows,]
  tmpdat2 = data.frame(
    exercise = tmpdat2$exercise
  )
  tmpdat2$set = NA
  for(za in c(1:nrow(tmpdat2))){
    tmpdat2$set[za] = sample(light_sets,1)
  }
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 1), ][,1],
    set = c(sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat2,tmpdat_card2,tmpdat_abs2
  ))
}


## build push
create_buildpush = function(){
  tmpdat_push = subset(exlist, category == 'push')
  tmpdat_compound = subset(tmpdat_push, group == 'compound')
  tmpdat_chest = subset(tmpdat_push, group == 'chest')
  tmpdat_shoulder = subset(tmpdat_push, group == 'shoulder')
  tmpdat_tricep = subset(tmpdat_push, group == 'tricep')
  tmpdat2 = data.frame(rbind(
    tmpdat_compound[sample(nrow(tmpdat_compound),7), ],
    tmpdat_chest[sample(nrow(tmpdat_chest),2), ],
    tmpdat_shoulder[sample(nrow(tmpdat_shoulder),5), ],
    tmpdat_tricep[sample(nrow(tmpdat_tricep),4), ]
  ))
  rows = sample(nrow(tmpdat2))
  tmpdat2 = tmpdat2[rows,]
  tmpdat2 = data.frame(
    exercise = tmpdat2$exercise
  )
  tmpdat2$set = NA
  for(za in c(1:nrow(tmpdat2))){
    tmpdat2$set[za] = sample(build_sets,1)
  }
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 1), ][,1],
    set = c(sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat2,tmpdat_card2,tmpdat_abs2
  ))
}

## light push
create_lightpush = function(){
  tmpdat_push = subset(exlist, category == 'push')
  tmpdat_compound = subset(tmpdat_push, group == 'compound')
  tmpdat_chest = subset(tmpdat_push, group == 'chest')
  tmpdat_shoulder = subset(tmpdat_push, group == 'shoulder')
  tmpdat_tricep = subset(tmpdat_push, group == 'tricep')
  tmpdat2 = data.frame(rbind(
    tmpdat_compound[sample(nrow(tmpdat_compound),7), ],
    tmpdat_chest[sample(nrow(tmpdat_chest),2), ],
    tmpdat_shoulder[sample(nrow(tmpdat_shoulder),5), ],
    tmpdat_tricep[sample(nrow(tmpdat_tricep),4), ]
  ))
  rows = sample(nrow(tmpdat2))
  tmpdat2 = tmpdat2[rows,]
  tmpdat2 = data.frame(
    exercise = tmpdat2$exercise
  )
  tmpdat2$set = NA
  for(za in c(1:nrow(tmpdat2))){
    tmpdat2$set[za] = sample(light_sets,1)
  }
  tmpdat_card = subset(exlist, category == 'cardio')
  tmpdat_abs = subset(exlist, category == 'abs')
  tmpdat_card2 = data.frame(
    exercise = tmpdat_card[sample(nrow(tmpdat_card), 1), ][,1],
    set = c(sample(cardio_minutes,1)))
  tmpdat_abs2 = data.frame(
    exercise = tmpdat_abs[sample(nrow(tmpdat_abs), 10), ][,1],
    set = c(sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1),
            sample(ab_reps,1),sample(ab_reps,1))
  )
  return(list(
    tmpdat2,tmpdat_card2,tmpdat_abs2
  ))
}

create_misc = function(){
  tmpdat_fullbody = subset(exlist, category == "full_body")
  tmpdat_misc = subset(exlist, category == "misc")
  tmpdat_fullbody2 = data.frame(
    exercise = tmpdat_fullbody[sample(nrow(tmpdat_fullbody), 2), ][,1],
    set = c(sample(build_sets,1),
            sample(build_sets,1)))
  tmpdat_misc2 = data.frame(
    exercise = tmpdat_misc[sample(nrow(tmpdat_misc), 1), ][,1],
    set = c(sample(build_sets,1)))
  tmpdat2 = data.frame(rbind(tmpdat_fullbody2,tmpdat_misc2))
  return(tmpdat2)
}









#deal with off/rehab
if(what_phase == 'off/rehab'){
  if(day == 'off'){
    wod = create_off()
  } else {
    wod = create_offRehab()
  }
}
if(what_phase == 'cardio'){
  if(day == 'off'){
    wod = create_off()
  } else {
    wod = create_cardio()
  }
}

if(what_phase == 'build'){
  if(day == 'off'){
    wod = create_off()
  } else if(day == 'push'){
    wod = create_buildpush()
  } else if(day == 'pull'){
    wod = create_buildpull()
  } else {
    wod = create_buildLegs()
  }
}

if(what_phase == 'light'){
  if(day == 'off'){
    wod = create_off()
  } else if(day == 'push'){
    wod = create_lightpush()
  } else if(day == 'pull'){
    wod = create_lightpull()
  } else {
    wod = create_lightLegs()
  }
}




