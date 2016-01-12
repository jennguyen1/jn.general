
############################################################################
# in_staff_student_course : merge district name to in_staff_student_course #
############################################################################

# subset xwalk to name and id
sub_district_xwalk <- subset(in_district_xwalk, ,select=c("district_id", "district_name"))

# setkey
setkey(sub_district_xwalk, district_id)
setkey(in_staff_student_course, district_id)

# merge to add district names
in_staff_student_course_v2  <- merge(in_staff_student_course, sub_district_xwalk, by="district_id", all.x=TRUE)

# trim sub_staff_student_course_v2 #change, this was below in the script
in_staff_student_course_v2[ , district_id:=str_trim(district_id, side=c("both"))]
in_staff_student_course_v2[ , course_id:=str_trim(course_id, side=c("both"))]

# change fix teacher id's in local id/ id switched districts
in_staff_student_course_v2[ , teacher_id:=str_trim(teacher_id, side=c("both"))]

# merge on teacher xwalk
in_staff_student_course_v2[,flag_staff_stud_course:=1]
sub_teacher_xwalk[,flag_xwalk:=1]
sub_staff_student_course <- merge(in_staff_student_course_v2, sub_teacher_xwalk, by=c("teacher_id", "district_id"), all.x=TRUE)

sub_staff_student_course[,alt_teacher_id:=as.character(alt_teacher_id)]
sub_staff_student_course[district_id %chin% c("NY280219", "NY280521", "NY441000", "NY580232", "NY580405"), teacher_id := alt_teacher_id]


##################################################################
# in_staff_student_course : create num_start_dates and end_dates #
##################################################################

# create student_district paste
sub_staff_student_course[,student_id:=paste0(district_id,"_",student_id)]
sub_staff_student_course[,teacher_id:=paste0(district_id,"_",teacher_id)]

# subset to reporting dates = 6/30/2015 #drops
sub_staff_student_course[,reporting_date:=as.Date(reporting_date, "%m/%d/%Y")]
sub_staff_student_course_v2 <- subset(sub_staff_student_course,reporting_date=="2015-06-30")

######################
# in_course : format #
######################

# add NY to districts without NY
in_course[!grepl("NY", district_id),district_id:=paste0("NY", district_id)]

# save original course_id
in_course[, course_id_orig:=course_id]

# create new_course_id
in_course[is.na(local_course_id) & !is.na(course_id), new_course_id:=course_id]
in_course[!is.na(local_course_id) & is.na(course_id), new_course_id:=local_course_id]
in_course[local_course_id == course_id, new_course_id:=course_id]

# drop course_id
sub_course <- subset(in_course, , select=-c(course_id))

# setname, so new_course_id becomes the course_id
setnames(sub_course, "new_course_id", "course_id")

# temp copy over sub_course to sub_course_v2 because no dup course xwalk step
sub_course_v2 <- copy(sub_course)