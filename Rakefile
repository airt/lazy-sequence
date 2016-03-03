
task default: %i[test]

def racket(file)
  sh "racket #{file}"
rescue => e
  puts e.message
end

task :test do
  Dir.glob("test/test*.scm").each do |t|
    racket t
  end
end
