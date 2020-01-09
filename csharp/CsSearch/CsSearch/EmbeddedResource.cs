using System;
using System.IO;
using System.Reflection;
using System.Text;

namespace CsSearch
{
    public static class EmbeddedResource
    {
        public static string GetResourceFileContents(string namespaceAndFileName)
        {
            try
            {
                using (var stream = typeof(EmbeddedResource).GetTypeInfo().Assembly.GetManifestResourceStream(namespaceAndFileName))
                using (var reader = new StreamReader(stream, Encoding.UTF8))
                    return reader.ReadToEnd();
            }

            catch(Exception)
            {
                //ApplicationProvider.WriteToLog<EmbeddedResource>().Error(exception.Message);
                throw new Exception($"Failed to read Embedded Resource {namespaceAndFileName}");
            }
        }
    }
}
