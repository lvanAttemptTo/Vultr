using System;
namespace GUI
{
    string APIKey = "vmgu1o6c6ihc";

    public class Bird
    {
        public string name { get; set; }
        public bool native { get; set; }
        public string status { get; set; }
        public int FindNearbySightings(float lat, float lng)
        {
            int value = 0;
            // call API

            return value;
        }
        public int sightings { get; set; }
        public float closestSighting { get; set; }
        
    }

    class Program
    {
        string APIKey = "vmgu1o6c6ihc";
        static void Main() {
            
            Bird bird = new Bird();
            bird.name = "House Sparrow";
            Console.WriteLine();

            var client = new HttpClient();
            var request = new HttpRequestMessage(HttpMethod.Get, "https://api.ebird.org/v2/data/obs/{{regionCode}}/recent/notable?detail=full");
            request.Headers.Add("X-eBirdApiToken", "{{x-ebirdapitoken}}");
            var response = client.Send(request);
            response.EnsureSuccessStatusCode();
            Console.WriteLine(response.Content.Read());

            // take in name of the bird and location

            // poll API to figure out if the bird is native and other information

            // output information to user
        }
    }
}