using System;
using System.IO;
using System.Reflection;
using System.Text;

namespace CsSearchTests
{
    public static class EmbeddedTestResource
    {
        public static string GetResourceFileContents(string namespaceAndFileName)
        {
            try
            {
                using var stream = typeof(EmbeddedTestResource).GetTypeInfo().Assembly
                    .GetManifestResourceStream(namespaceAndFileName);
                using var reader = new StreamReader(stream!, Encoding.UTF8);
                return reader.ReadToEnd();
            }
            catch(Exception)
            {
                throw new Exception($"Failed to read Embedded Resource {namespaceAndFileName}");
            }
        }
    }
}
