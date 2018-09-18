#' Count syllables
#'
#' How many syllables are in an English word?
#' @param ortho A character value (an English word or name)
#' @keywords text
#' @return tot_syls The number of syllables in the string
#' @export
#' @examples 
#' english_syllable_count('Mississippi')
#' @seealso Full credit for the development of this function goes to Tyler Kendall at the University of Oregon http://lingtools.uoregon.edu/scripts/english_syllable_counter-102.R

english_syllable_count <- function(ortho) {
  
  # Can add words to these lists of 2 syllable and 3 syllable 'exceptions'
  # Note that final -e is stripped before checking these lists!
  Specials.2 <- c('every', 'different', 'family', 'girl', 'girls', 'world', 'worlds', 'bein', 'being', 'something', 'mkay', 'mayb')
  Specials.3 <- c('anyon', 'everyon') # final -e is dropped	
  
  # Regular Expression exceptions
  # SubSyl - remove a syllable from the count for each sub-string match
  SubSyl <- c('cial',
              'tia',
              'cius',
              'cious',
              'giu',              # belgium!
              'ion',
              'iou',
              '^every',           # every, but also everything, everybody
              'sia$',
              '.ely$',            # absolutely! (but not ely!)
              '[^szaeiou]es$',    # fates, but not sasses
              '[^tdaeiou]ed$',    # trapped, but not fated
              '^ninet',           # nineteen, ninety
              '^awe'				# awesome
  )
  
  # AddSyl - add a syllable to the count for each sub-string match
  AddSyl <- c('ia',
              'rie[rt]',
              'dien',
              'ieth',
              'iu',
              'io',
              'ii',
              'ienc',	      # ambience, science, ...
              'les?$',
              '[aeiouym][bp]l$',  # -Vble, plus -mble and -Vple
              '[aeiou]{3}',       # agreeable
              'ndl(ed)?$',        # handle, handled
              'mpl(ed)?$',	    # trample, trampled
              '^mc',				# McEnery
              'ism$',             # -isms
              '([^aeiouy])\\1l(ed)?$',  # middle twiddle battle bottle, etc.
              '[^l]lien',         # alien, salient [1]
              '^coa[dglx].',      # [2]
              '[^gq]ua[^aeiou]',  # i think this fixes more than it breaks
              '[sd]nt$',          # couldn't, didn't, hasn't, wasn't,...
              '\\wshes$',          # add one back for esh (since it's -'d)
              '\\wches$',          #  and for affricate (witches)
              '\\wges$',           #  and voiced (ages)
              '\\wces$',	      #  and sibilant 'c's (places)
              '\\w[aeiouy]ing[s]?$'   # vowels before -ing = hiatus
  )
  
  tot_syls <- 0
  ortho.l <- tolower(ortho)
  stripchars <- "[:'\\[\\]]"
  ortho.cl <- gsub(stripchars, "", ortho.l, perl=T)
  spacechars <- "[\\W_]" # replace other non-word chars with space
  ortho.cl <- gsub(spacechars, " ", ortho.cl, perl=T)
  ortho.vec <- unlist(strsplit(ortho.cl, " ", perl=T))
  ortho.vec <- ortho.vec[ortho.vec!=""]
  for (w in ortho.vec) {
    w <- gsub("e$", "", w, perl=T) # strip final -e
    syl <- 0
    # is word in the 2 syllable exception list?
    if (w %in% Specials.2) {
      syl <- 2
      
      # is word in the 3 syllable exception list?
    } else if (w %in% Specials.3) {
      syl <- 3
      
      # if not, than check the different parts...
    } else {
      for (pat in SubSyl) {
        if (length(grep(pat, w, perl=T))>=1) 
          syl <- syl - 1
      }
      for (pat in AddSyl) {
        if (length(grep(pat, w, perl=T))>=1) 
          syl <- syl + 1
      }
      if (nchar(w)==1) {
        syl <- 1
      } else {
        chnk <- unlist(strsplit(w, "[^aeiouy:]+"))
        chnk <- chnk[chnk!=""]
        syl <- syl + length(chnk)
        if (syl==0) syl <- 1
      }
    }
    tot_syls <- tot_syls + syl
  }
  tot_syls
}