function count(symbol) {
 print "\tsub sp, sp, #32"
 print "\tstp x16, x17, [sp, #0]"
 print "\tstr x30, [sp, #16]"
 print "\tadrp x16, _" symbol "@GOTPAGE"
 print "\tldr x16, [x16, _" symbol "@GOTPAGEOFF]"
 print "\tldr x17, [x16, #0]"
 print "\tadd x17, x17, #1"
 print "\tstr x17, [x16, #0]"
 print "\tldp x16, x17, [sp, #0]"
 print "\tldr x30, [sp, #16]"
 print "\tadd sp, sp, #32"
}
{
 gsub(/_resetRegion/,"_diag_resetRegion")
 if ($0=="_mlkit_arm64_reset_preserving:") shared=1
 if (shared && $0 ~ /^[[:space:]]+ret$/) {count("diag_shared_hits"); shared=0}
 print
 if ($0=="_mlkit_arm64_reset_preserving:") count("diag_shared_entries")
 if ($0=="_mlkit_arm64_reset_slow:") count("diag_slow_entries")
 if ($0=="_mlkit_arm64_reset:") count("diag_legacy_entries")
 if ($0=="\tstr x30, [x17, #0]" && previous=="\tadd x30, x30, #16" &&
     prev2=="\tand x30, x30, #-32" && prev3=="\tldr x30, [x17, #8]" &&
     prev4=="\tand x17, x16, #-4") count("diag_site_hits")
 prev4=prev3; prev3=prev2; prev2=previous; previous=$0
}
