process_recommendation = function () 
{
  challenges = read.csv("challenges.csv")
  submissions = read.csv("submissions.csv")
  
  
  count = 0
  for (hacker in unique (submissions$hacker_id)) {
    res = c()
    
    count = count + 1
    print (paste (count, "/", length(unique(submissions$hacker_id))))
    
    
    for (challenge in unique (submissions[submissions$hacker_id == hacker,]$challenge_id)) {
      
      if (!is.element(1, submissions[submissions$hacker_id == hacker & submissions$challenge_id == challenge,]$solved)) {
        res = c(res, as.character(submissions[submissions$hacker_id == hacker & submissions$challenge_id == challenge,]$challenge_id))
      }
      
      domain = challenges[challenges$challenge_id == challenge,]$domain[1]
      subdomain = challenges[challenges$challenge_id == challenge,]$subdomain[1]
      
      potential_challenges = challenges[challenges$subdomain == subdomain & challenges$domain == domain,]$challenge_id
      # print (potential_challenges)
      for (challenge in potential_challenges) {
        if (!is.element(1, submissions[submissions$hacker_id == hacker & submissions$challenge_id == challenge,]$solved)) {
          # print ("1")
          res = c(res, as.character(challenge))
        }
      }
      
      if (length(res) < 10) {
        potential_challenges = challenges[challenges$subdomain == subdomain,]$challenge_id
        # print (potential_challenges)
        for (challenge in potential_challenges) {
          if (!is.element(1, submissions[submissions$hacker_id == hacker & submissions$challenge_id == challenge,]$solved)) {
            # print ("1")
            res = c(res, as.character(challenge))
          }
        }
      }
      
      if (length(res) < 10) {
        potential_challenges = challenges[challenges$domain == domain,]$challenge_id
        # print (potential_challenges)
        for (challenge in potential_challenges) {
          if (!is.element(1, submissions[submissions$hacker_id == hacker & submissions$challenge_id == challenge,]$solved)) {
            # print ("2")
            res = c(res, as.character(challenge))
          }
        }
      }
      
    }
    
    res = unique(res)
    res = res[1:10]
    
    res = c(as.character(hacker), res)
    
    out = paste(res, collapse = ",")
    
    write (out, file = "recommendation.csv", sep = ",", append = TRUE)
    # write.csv(res, file = "recommendation.csv", sep = ",", append = TRUE)
    
    # print (out)
    print (length(res))
    print ("----")
  }
}